from pyray import *
import settings
from map_engine import MapEngine
from editor_ui import EditorUI
from map_data import MapData
import fonctionetprocedure


def main():
    init_window(settings.SCREEN_WIDTH, settings.SCREEN_HEIGHT, settings.TITLE)
    set_target_fps(settings.FPS)

    map_engine = MapEngine()
    ui = EditorUI()
    map_data = MapData()

    liste_fichiers, liste_fichiers_str = fonctionetprocedure.lister_cartes_disponibles()
    liste_projets, liste_projets_str = fonctionetprocedure.lister_projets_sauvegardes()

    current_mode = settings.MODE_EDITION
    current_test_submode = settings.TEST_SUB_INSPEC
    current_tool = settings.TOOL_NODE
    log_message = "Bienvenue."

    selected_node = None
    selected_edge = None
    start_node_id_for_edge = None
    dragging_edge = None
    node_to_delete = None
    edge_to_delete = None

    inspection_info = ""
    inspected_edge = None
    inspected_node = None

    # Variables Simulation Manuel
    sim_current_node = None
    sim_current_autonomy = 0
    sim_path_edges = []
    sim_manual_edge_info = ""

    # Variables Simulation AUTO
    sim_auto_start = None
    sim_auto_end = None
    sim_auto_path = []  # La liste des edges calculée
    last_weight_val = -1  # Pour détecter changement
    last_auto_val = -1

    while not window_should_close():

        mouse_pos_screen = get_mouse_position()
        mouse_in_view = (mouse_pos_screen.x < settings.VIEW_WIDTH and mouse_pos_screen.y < settings.VIEW_HEIGHT)
        mouse_w = map_engine.get_mouse_world_pos()

        # --- INPUT EDITION (DRAG) ---
        if current_mode == settings.MODE_EDITION and node_to_delete is None and edge_to_delete is None:
            if is_mouse_button_pressed(
                    MOUSE_BUTTON_LEFT) and current_tool == settings.TOOL_EDGE and mouse_in_view and not ui.show_list:
                edge_under_mouse = map_data.get_edge_handle_at(mouse_w.x, mouse_w.y)
                if edge_under_mouse:
                    dragging_edge = edge_under_mouse
                    selected_edge = edge_under_mouse
                    ui.sync_ui_with_edge(selected_edge)
                    log_message = "Deplacement Voie..."

            if dragging_edge:
                if is_mouse_button_down(MOUSE_BUTTON_LEFT):
                    dragging_edge.handle_x = int(mouse_w.x)
                    dragging_edge.handle_y = int(mouse_w.y)
                else:
                    dragging_edge = None
                    log_message = "Voie modifiee."

        # --- UPDATE MOTEUR ---
        is_engine_locked = (node_to_delete is not None) or (edge_to_delete is not None) or (dragging_edge is not None)
        map_engine.update(locked=is_engine_locked)

        # --- LOGIQUE EDITION ---
        if not is_engine_locked and current_mode == settings.MODE_EDITION:
            # (Reste inchangé...)
            if current_tool == settings.TOOL_NODE:
                wheel = get_mouse_wheel_move()
                if wheel != 0:
                    inc = 1 if wheel > 0 else -1
                    if ui.selected_shape[0] == settings.SHAPE_CIRCLE:
                        ui.radius_ptr[0] = max(1, ui.radius_ptr[0] + inc)
                    else:
                        ui.width_ptr[0] = max(1, ui.width_ptr[0] + inc)

                if selected_node is not None:
                    if ui.is_name_being_edited(): selected_node.name = ui.get_name_str()
                    selected_node.vp = ui.vp_ptr[0]
                    selected_node.height = ui.height_val_ptr[0]
                    selected_node.terrain = ui.terrain_val_ptr[0]
                    selected_node.object_id = ui.obj_val_ptr[0]
                    selected_node.contenance = ui.cap_ptr[0]

                    if selected_node.shape_type == settings.SHAPE_CIRCLE:
                        selected_node.dim1 = ui.radius_ptr[0]
                    else:
                        selected_node.dim1 = ui.width_ptr[0]
                        selected_node.dim2 = ui.height_ptr[0]

                    if is_key_pressed(KEY_RIGHT): selected_node.x += 1
                    if is_key_pressed(KEY_LEFT):  selected_node.x -= 1
                    if is_key_pressed(KEY_DOWN):  selected_node.y += 1
                    if is_key_pressed(KEY_UP):    selected_node.y -= 1

            if current_tool == settings.TOOL_EDGE:
                if selected_edge is not None:
                    selected_edge.type_voie = ui.edge_type_ptr[0]
                    selected_edge.direction = ui.edge_dir_ptr[0]
                    selected_edge.vitesse = ui.edge_vit_ptr[0]
                    selected_edge.tonnage = ui.edge_ton_ptr[0]
                    selected_edge.nb_vehicules = ui.edge_veh_ptr[0]
                    selected_edge.poids_max = ui.edge_pmax_ptr[0]
                    selected_edge.nature_sol = ui.edge_nat_ptr[0]
                    selected_edge.viabilite = ui.edge_via_ptr[0]
                    selected_edge.color = settings.EDGE_TYPE_COLORS.get(selected_edge.type_voie,
                                                                        settings.DEFAULT_EDGE_COLOR)

        # --- DESSIN ---
        begin_drawing()
        clear_background(settings.BG_COLOR)

        map_engine.draw()

        if current_mode == settings.MODE_EDITION:
            begin_mode_2d(map_engine.camera)
            map_data.draw_editor(
                show_nodes=ui.show_nodes_ptr[0],
                show_edges=ui.show_edges_ptr[0],
                show_handles=ui.show_handles_ptr[0]
            )

            if selected_node and selected_node != node_to_delete and ui.show_nodes_ptr[0]:
                if selected_node.shape_type == settings.SHAPE_CIRCLE:
                    draw_circle_lines(selected_node.x, selected_node.y, selected_node.dim1, ORANGE)
                else:
                    l = int(selected_node.x - selected_node.dim1 / 2)
                    t = int(selected_node.y - selected_node.dim2 / 2)
                    draw_rectangle_lines(l, t, selected_node.dim1, selected_node.dim2, ORANGE)

            if selected_edge and selected_edge != edge_to_delete and ui.show_handles_ptr[0]:
                hs = settings.HANDLE_SIZE + 4
                draw_rectangle_lines(selected_edge.handle_x - int(hs / 2), selected_edge.handle_y - int(hs / 2), hs, hs,
                                     ORANGE)

            if start_node_id_for_edge is not None:
                start_node = map_data.get_node_by_id(start_node_id_for_edge)
                if start_node:
                    draw_circle_lines(start_node.x, start_node.y, start_node.dim1 + 5, settings.START_NODE_COLOR)
                    draw_line(start_node.x, start_node.y, int(mouse_w.x), int(mouse_w.y), fade(BLACK, 0.5))

            if current_tool == settings.TOOL_NODE and not ui.show_list and not selected_node and not node_to_delete:
                is_circle = (ui.selected_shape[0] == settings.SHAPE_CIRCLE)
                if is_circle:
                    draw_circle_lines(int(mouse_w.x), int(mouse_w.y), ui.radius_ptr[0], settings.GHOST_COLOR)
                else:
                    w = ui.width_ptr[0]
                    h = ui.height_ptr[0]
                    draw_rectangle_lines(int(mouse_w.x - w / 2), int(mouse_w.y - h / 2), w, h, settings.GHOST_COLOR)

            end_mode_2d()

        elif current_mode == settings.MODE_TEST:
            begin_mode_2d(map_engine.camera)
            # Affiche uniquement les poignées
            map_data.draw_editor(show_nodes=False, show_edges=False, show_handles=True)

            if current_test_submode == settings.TEST_SUB_INSPEC:
                if inspected_edge: map_data.draw_single_edge_lines(inspected_edge)
                if inspected_node:
                    col = settings.CYAN
                    if inspected_node.shape_type == settings.SHAPE_CIRCLE:
                        draw_circle_lines(inspected_node.x, inspected_node.y, inspected_node.dim1, col)
                    else:
                        l = int(inspected_node.x - inspected_node.dim1 / 2)
                        t = int(inspected_node.y - inspected_node.dim2 / 2)
                        draw_rectangle_lines(l, t, inspected_node.dim1, inspected_node.dim2, col)

            elif current_test_submode == settings.TEST_SUB_MANUAL:
                map_data.draw_test_path(sim_path_edges)
                if sim_current_node:
                    draw_circle(sim_current_node.x, sim_current_node.y, 15, RED)
                    draw_circle_lines(sim_current_node.x, sim_current_node.y, 16, BLACK)

            # NOUVEAU : DESSIN MODE AUTO (Rouge + Vert)
            elif current_test_submode == settings.TEST_SUB_AUTO:
                # Départ
                if sim_auto_start:
                    draw_circle_lines(sim_auto_start.x, sim_auto_start.y, sim_auto_start.dim1 + 5,
                                      settings.START_NODE_COLOR)
                # Arrivée
                if sim_auto_end:
                    draw_circle_lines(sim_auto_end.x, sim_auto_end.y, sim_auto_end.dim1 + 5, RED)

                # Trajet calculé
                if sim_auto_path:
                    # On demande au map_data de le dessiner selon l'autonomie
                    autonomy = ui.sim_auto_ptr[0]
                    map_data.draw_auto_path(sim_auto_path, autonomy)

            end_mode_2d()

        # UI DRAW
        new_mode, new_test_sub, new_tool, new_msg, selected_index, selection_type, save_requested, reset_sim_req = ui.draw(
            current_mode, current_test_submode, current_tool, log_message, map_engine,
            liste_fichiers_str, liste_projets_str, sim_manual_edge_info, inspection_info
        )

        # Reset SIM
        if reset_sim_req:
            # Manuel
            sim_current_node = None
            sim_path_edges = []
            sim_current_autonomy = ui.sim_auto_ptr[0]
            sim_manual_edge_info = ""
            # Auto
            sim_auto_start = None
            sim_auto_end = None
            sim_auto_path = []
            new_msg = "Simulation reinitialisee."

        # Changement Mode
        if new_mode != current_mode:
            current_mode = new_mode
            selected_node = None
            selected_edge = None
            inspected_edge = None
            inspected_node = None
            inspection_info = ""
            sim_current_node = None
            sim_manual_edge_info = ""
            sim_auto_start = None
            sim_auto_end = None
            sim_auto_path = []

        current_test_submode = new_test_sub

        # LOGIQUE RECALCUL AUTOMATIQUE (Si poids change)
        if current_mode == settings.MODE_TEST and current_test_submode == settings.TEST_SUB_AUTO:
            curr_weight = ui.sim_weight_ptr[0]
            # Si le poids a changé, on doit recalculer car certains ponts peuvent devenir interdits
            if sim_auto_start and sim_auto_end and curr_weight != last_weight_val:
                log_message = "Recalcul itineraire..."
                sim_auto_path = map_data.find_shortest_path(sim_auto_start.id, sim_auto_end.id, curr_weight)
                if sim_auto_path is None:
                    log_message = "Aucun chemin possible (Poids ?)"
                    sim_auto_path = []
                last_weight_val = curr_weight

        if new_tool != current_tool:
            selected_node = None
            selected_edge = None
            start_node_id_for_edge = None
            node_to_delete = None
            edge_to_delete = None
            dragging_edge = None

        current_tool = new_tool
        log_message = new_msg

        if save_requested:
            resultat_sauvegarde = fonctionetprocedure.sauvegarder_projet(map_engine.current_map_name, map_data)
            log_message = resultat_sauvegarde
            liste_projets, liste_projets_str = fonctionetprocedure.lister_projets_sauvegardes()

        if current_mode == settings.MODE_EDITION:
            # (Logique Edition Inchangée...)
            if node_to_delete is None and edge_to_delete is None and map_engine.was_clicked and not ui.show_list and mouse_in_view and not dragging_edge:
                if current_tool == settings.TOOL_NODE:
                    clicked_node = map_data.get_node_at(mouse_w.x, mouse_w.y)
                    if clicked_node:
                        selected_node = clicked_node
                        ui.sync_ui_with_node(selected_node)
                        new_msg = f"Selection Noeud ID {selected_node.id}"
                    else:
                        shape = ui.selected_shape[0]
                        d1 = ui.radius_ptr[0] if shape == settings.SHAPE_CIRCLE else ui.width_ptr[0]
                        d2 = 0 if shape == settings.SHAPE_CIRCLE else ui.height_ptr[0]
                        name = ""
                        vp = ui.vp_ptr[0]
                        h = ui.height_val_ptr[0]
                        terr = ui.terrain_val_ptr[0]
                        obj = ui.obj_val_ptr[0]
                        cap = ui.cap_ptr[0]
                        map_data.add_node(mouse_w.x, mouse_w.y, shape, d1, d2, name, vp, h, terr, obj, cap)
                        ui.set_name_str("")
                        selected_node = None
                        new_msg = "Nouveau noeud cree."

                elif current_tool == settings.TOOL_EDGE:
                    clicked_edge = map_data.get_edge_handle_at(mouse_w.x, mouse_w.y)
                    if clicked_edge:
                        selected_edge = clicked_edge
                        ui.sync_ui_with_edge(selected_edge)
                        new_msg = f"Voie selectionnee (ID {selected_edge.id})"
                    else:
                        clicked_node = map_data.get_node_at(mouse_w.x, mouse_w.y)
                        if clicked_node:
                            if start_node_id_for_edge is None:
                                start_node_id_for_edge = clicked_node.id
                                new_msg = "Depart Voie selectionne."
                            else:
                                if clicked_node.id != start_node_id_for_edge:
                                    node_a = map_data.get_node_by_id(start_node_id_for_edge)
                                    node_b = clicked_node
                                    t = ui.edge_type_ptr[0]
                                    d = ui.edge_dir_ptr[0]
                                    v = ui.edge_vit_ptr[0]
                                    tn = ui.edge_ton_ptr[0]
                                    vh = ui.edge_veh_ptr[0]
                                    pm = ui.edge_pmax_ptr[0]
                                    nat = ui.edge_nat_ptr[0]
                                    via = ui.edge_via_ptr[0]
                                    map_data.add_edge(node_a, node_b, t, d, v, tn, vh, pm, nat, via)
                                    new_msg = "Voie creee."
                                    start_node_id_for_edge = None
                                else:
                                    new_msg = "Annulation (meme noeud)."
                                    start_node_id_for_edge = None
                        else:
                            if not selected_edge:
                                start_node_id_for_edge = None
                                selected_edge = None

                elif current_tool == settings.TOOL_DELETE:
                    clicked_edge = map_data.get_edge_handle_at(mouse_w.x, mouse_w.y)
                    if clicked_edge:
                        edge_to_delete = clicked_edge
                        new_msg = "Confirmez suppression Voie..."
                    else:
                        clicked_node = map_data.get_node_at(mouse_w.x, mouse_w.y)
                        if clicked_node:
                            node_to_delete = clicked_node
                            new_msg = "Confirmez suppression Noeud..."

            if node_to_delete or edge_to_delete:
                msg = ""
                if node_to_delete:
                    msg = f"Supprimer le noeud {node_to_delete.id}\n({node_to_delete.name}) ?"
                elif edge_to_delete:
                    msg = f"Supprimer la voie ID {edge_to_delete.id} ?"
                result = ui.draw_confirmation_popup(msg)
                if result == 1:
                    if node_to_delete:
                        map_data.delete_node(node_to_delete)
                        log_message = f"Noeud {node_to_delete.id} supprime."
                        node_to_delete = None
                        selected_node = None
                    elif edge_to_delete:
                        map_data.delete_edge(edge_to_delete)
                        log_message = f"Voie {edge_to_delete.id} supprimee."
                        edge_to_delete = None
                        selected_edge = None
                elif result == 2 or result == 0:
                    node_to_delete = None
                    edge_to_delete = None
                    log_message = "Suppression annulee."

        elif current_mode == settings.MODE_TEST:

            if current_test_submode == settings.TEST_SUB_INSPEC:
                if map_engine.was_clicked and mouse_in_view:
                    clk_edge = map_data.get_edge_handle_at(mouse_w.x, mouse_w.y)
                    if clk_edge:
                        inspected_edge = clk_edge
                        inspected_node = None
                        inspection_info = (
                            f"VOIE ID: {clk_edge.id}\n"
                            f"De Node {clk_edge.node_a} a {clk_edge.node_b}\n"
                            f"Cout: {clk_edge.vitesse} | Dir: {clk_edge.direction}\n"
                            f"Nature: {clk_edge.nature_sol} | Viab: {clk_edge.viabilite}\n"
                            f"----------------\n"
                            f"Ton: {clk_edge.tonnage} | Veh: {clk_edge.nb_vehicules}\n"
                            f"Max: {clk_edge.poids_max}"
                        )
                    else:
                        clk_node = map_data.get_node_at(mouse_w.x, mouse_w.y)
                        if clk_node:
                            inspected_edge = None
                            inspected_node = clk_node
                            inspection_info = (
                                f"NOEUD ID: {clk_node.id}\n"
                                f"Nom: {clk_node.name}\n"
                                f"Pos: {clk_node.x}, {clk_node.y}\n"
                                f"----------------\n"
                                f"PV: {clk_node.vp} | Haut: {clk_node.height}\n"
                                f"Terr: {clk_node.terrain} | Obj: {clk_node.object_id}\n"
                                f"Cap: {clk_node.contenance}"
                            )

            elif current_test_submode == settings.TEST_SUB_MANUAL:
                if map_engine.was_clicked and mouse_in_view:
                    clk_node = map_data.get_node_at(mouse_w.x, mouse_w.y)
                    if clk_node:
                        sim_current_node = clk_node
                        sim_path_edges = []
                        sim_current_autonomy = ui.sim_auto_ptr[0]
                        sim_manual_edge_info = ""
                        log_message = f"Vehicule place sur {clk_node.name}."
                    else:
                        clk_edge = map_data.get_edge_handle_at(mouse_w.x, mouse_w.y)
                        if clk_edge and sim_current_node:
                            target_node_id = map_data.get_other_node_id(clk_edge, sim_current_node.id)

                            sim_manual_edge_info = (
                                f"VOIE: {clk_edge.id} (Type {clk_edge.type_voie})\n"
                                f"Cout: {clk_edge.vitesse} | Max Poids: {clk_edge.poids_max}\n"
                                f"Viabilite: {clk_edge.viabilite}"
                            )

                            if target_node_id is not None:
                                veh_weight = ui.sim_weight_ptr[0]
                                if veh_weight > clk_edge.poids_max:
                                    log_message = f"ECHEC: Trop lourd ({veh_weight} > {clk_edge.poids_max})"
                                else:
                                    cost = clk_edge.vitesse
                                    if cost > sim_current_autonomy:
                                        log_message = f"ECHEC: Pas assez d'autonomie ({sim_current_autonomy} < {cost})"
                                    else:
                                        sim_current_autonomy -= cost
                                        sim_path_edges.append(clk_edge)
                                        sim_current_node = map_data.get_node_by_id(target_node_id)
                                        log_message = f"Mouvement OK. Reste {sim_current_autonomy}."
                            else:
                                log_message = "Erreur: Voie non connectee."

            # NOUVEAU : CLIC POUR MODE AUTO
            elif current_test_submode == settings.TEST_SUB_AUTO:
                if map_engine.was_clicked and mouse_in_view:
                    clk_node = map_data.get_node_at(mouse_w.x, mouse_w.y)
                    if clk_node:
                        # Si on n'a pas de départ, ou si on recommence
                        if sim_auto_start is None or (sim_auto_start and sim_auto_end):
                            sim_auto_start = clk_node
                            sim_auto_end = None
                            sim_auto_path = []
                            log_message = "Depart selectionne. Cliquez arrivee."
                        else:
                            # C'est l'arrivée
                            sim_auto_end = clk_node
                            weight = ui.sim_weight_ptr[0]
                            log_message = "Calcul itineraire..."

                            # APPEL DIJKSTRA
                            path = map_data.find_shortest_path(sim_auto_start.id, sim_auto_end.id, weight)
                            if path is not None:
                                sim_auto_path = path
                                log_message = f"Chemin trouve ({len(path)} troncons)."
                            else:
                                sim_auto_path = []
                                log_message = "AUCUN CHEMIN VALIDE !"

        if selected_index is not None and selected_index >= 0:
            if selection_type == "FILE":
                if selected_index < len(liste_fichiers):
                    map_engine.load_map(liste_fichiers[selected_index])
                    map_data.reset()
                    selected_node = None
                    node_to_delete = None
                    selected_edge = None
                    start_node_id_for_edge = None
                    dragging_edge = None
                    log_message = "Nouvelle carte."
                    ui.close_selector()

            elif selection_type == "PROJECT":
                if selected_index < len(liste_projets):
                    nom_projet = liste_projets[selected_index]
                    map_engine.load_project(nom_projet)
                    loaded_nodes, next_nid = fonctionetprocedure.charger_donnees_projet(nom_projet)
                    map_data.nodes = loaded_nodes
                    map_data.next_node_id = next_nid
                    loaded_edges, next_eid = fonctionetprocedure.charger_voies_projet(nom_projet)
                    map_data.edges = loaded_edges
                    map_data.next_edge_id = next_eid
                    selected_node = None
                    node_to_delete = None
                    selected_edge = None
                    start_node_id_for_edge = None
                    dragging_edge = None
                    log_message = f"Projet {nom_projet} ouvert."
                    ui.close_selector()

        end_drawing()

    map_engine.unload()
    close_window()


if __name__ == "__main__":
    main()