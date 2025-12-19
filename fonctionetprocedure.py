import os
import shutil
import settings
from pyray import Color
from map_data import Node, Edge

"""
GESTION FICHIERS ET SAUVEGARDE
Contient les fonctions pour scanner les dossiers, écrire et lire les CSV.
"""


def lister_cartes_disponibles():
    path = settings.RESOURCE_DIR
    if not os.path.exists(path):
        os.makedirs(path)
        return [], ""

    extensions_valides = ('.png', '.jpg', '.jpeg', '.bmp')
    fichiers = [f for f in os.listdir(path) if f.lower().endswith(extensions_valides)]
    fichiers.sort()

    raygui_str = ";".join(fichiers)
    return fichiers, raygui_str


def lister_projets_sauvegardes():
    path = settings.SAVED_DIR
    if not os.path.exists(path):
        os.makedirs(path)
        return [], ""

    projets = [d for d in os.listdir(path) if os.path.isdir(os.path.join(path, d))]
    projets.sort()

    raygui_str = ";".join(projets)
    return projets, raygui_str


def int_to_color(val):
    """Reconstruit une couleur depuis un entier ou renvoie une couleur par defaut"""
    try:
        r = (val >> 24) & 0xFF
        g = (val >> 16) & 0xFF
        b = (val >> 8) & 0xFF
        a = 255  # Force l'opacité
        return Color(r, g, b, a)
    except:
        return settings.DEFAULT_EDGE_COLOR


def sauvegarder_projet(nom_image_complet, map_data):
    if not nom_image_complet:
        return "Erreur: Aucune carte chargée."

    nom_sans_ext = os.path.splitext(nom_image_complet)[0]
    dossier_cible = os.path.join(settings.SAVED_DIR, nom_sans_ext)

    if not os.path.exists(dossier_cible):
        os.makedirs(dossier_cible)

    # 1. Copie Image
    src_image = os.path.join(settings.RESOURCE_DIR, nom_image_complet)
    dst_image = os.path.join(dossier_cible, nom_image_complet)
    try:
        if os.path.exists(src_image):
            shutil.copy2(src_image, dst_image)
    except Exception as e:
        print(f"Erreur copie image: {e}")

    # 2. Ecriture NOEUDS
    path_nodes = os.path.join(dossier_cible, "dataGraph.csv")
    try:
        with open(path_nodes, 'w', encoding='utf-8') as f:
            f.write("ID;X;Y;SHAPE;DIM1;DIM2;NAME;VP;HEIGHT;TERRAIN;OBJ;CAP\n")
            for node in map_data.nodes:
                line = (
                    f"{node.id};{node.x};{node.y};{node.shape_type};"
                    f"{node.dim1};{node.dim2};{node.name};{node.vp};"
                    f"{node.height};{node.terrain};{node.object_id};{node.contenance}\n"
                )
                f.write(line)
    except Exception as e:
        return f"Erreur Noeuds: {str(e)}"

    # 3. Ecriture VOIES (Format Robuste 4 entiers couleurs)
    path_edges = os.path.join(dossier_cible, "voie.csv")
    try:
        with open(path_edges, 'w', encoding='utf-8') as f:
            f.write("ID;NODE_A;NODE_B;HANDLE_X;HANDLE_Y;TYPE;DIR;VIT;TON;VEH;PMAX;NAT;VIA;R;G;B;A\n")

            for edge in map_data.edges:
                r, g, b, a = 0, 0, 0, 255

                # Gestion souple Tuple vs Object Color
                try:
                    r = int(edge.color.r)
                    g = int(edge.color.g)
                    b = int(edge.color.b)
                    a = int(edge.color.a)
                except AttributeError:
                    try:
                        r = int(edge.color[0])
                        g = int(edge.color[1])
                        b = int(edge.color[2])
                        a = int(edge.color[3]) if len(edge.color) > 3 else 255
                    except:
                        pass

                line = (
                    f"{edge.id};{edge.node_a};{edge.node_b};{edge.handle_x};{edge.handle_y};"
                    f"{edge.type_voie};{edge.direction};{edge.vitesse};{edge.tonnage};"
                    f"{edge.nb_vehicules};{edge.poids_max};{edge.nature_sol};{edge.viabilite};"
                    f"{r};{g};{b};{a}\n"
                )
                f.write(line)
    except Exception as e:
        print(f"CRASH SAVE VOIES: {e}")
        return f"Erreur Voies: {str(e)}"

    return f"Projet '{nom_sans_ext}' sauvegarde."


def charger_donnees_projet(nom_projet):
    """Charge les Noeuds depuis CSV"""
    path_csv = os.path.join(settings.SAVED_DIR, nom_projet, "dataGraph.csv")
    nodes_list = []
    max_id = -1

    if not os.path.exists(path_csv): return [], 0

    try:
        with open(path_csv, 'r', encoding='utf-8') as f:
            lines = f.readlines()
            for i in range(1, len(lines)):
                line = lines[i].strip()
                if not line: continue
                parts = line.split(';')
                if len(parts) < 12: continue

                uid = int(parts[0])
                x = int(parts[1])
                y = int(parts[2])
                shape = int(parts[3])
                dim1 = int(parts[4])
                dim2 = int(parts[5])
                name = parts[6]
                vp = int(parts[7])
                height = int(parts[8])
                terrain = int(parts[9])
                obj_id = int(parts[10])
                cap = int(parts[11])

                new_node = Node(uid, x, y, shape, dim1, dim2, name, vp, height, terrain, obj_id, cap)
                nodes_list.append(new_node)
                if uid > max_id: max_id = uid

    except Exception as e:
        print(f"Erreur chargement Noeuds: {e}")
        return [], 0

    return nodes_list, max_id + 1


def charger_voies_projet(nom_projet):
    """Charge les Voies depuis CSV (Compatible vieux formats)"""
    path_csv = os.path.join(settings.SAVED_DIR, nom_projet, "voie.csv")
    edges_list = []
    max_id = -1

    if not os.path.exists(path_csv): return [], 0

    try:
        with open(path_csv, 'r', encoding='utf-8') as f:
            lines = f.readlines()
            for i in range(1, len(lines)):
                line = lines[i].strip()
                if not line: continue
                parts = line.split(';')

                # Check format minimum
                if len(parts) < 15: continue

                try:
                    uid = int(parts[0])
                    node_a = int(parts[1])
                    node_b = int(parts[2])
                    hx = int(parts[3])
                    hy = int(parts[4])
                    typ = int(parts[5])
                    direc = int(parts[6])
                    vit = int(parts[7])
                    ton = int(parts[8])
                    veh = int(parts[9])
                    pmax = int(parts[10])
                    nat = int(parts[11])
                    via = int(parts[12])

                    # On force la couleur basée sur le TYPE défini dans settings pour la cohérence
                    color = settings.EDGE_TYPE_COLORS.get(typ, settings.DEFAULT_EDGE_COLOR)

                    new_edge = Edge(uid, node_a, node_b, hx, hy, typ, direc, vit, ton, veh, pmax, nat, via, color)
                    edges_list.append(new_edge)
                    if uid > max_id: max_id = uid
                except ValueError as ve:
                    print(f"Ligne {i} erreur valeur: {ve}")
                    continue

    except Exception as e:
        print(f"Erreur chargement Voies: {e}")
        return [], 0

    return edges_list, max_id + 1