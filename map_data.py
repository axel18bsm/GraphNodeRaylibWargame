from pyray import *
import settings
import heapq  # Nécessaire pour l'algorithme de Dijkstra


class Node:
    def __init__(self, uid, x, y, shape_type, dim1, dim2=0, name="", vp=0, height=0, terrain=1, obj_id=1, contenance=0):
        self.id = uid
        self.x = int(x)
        self.y = int(y)
        self.shape_type = shape_type
        self.dim1 = int(dim1)
        self.dim2 = int(dim2)
        self.name = name
        self.vp = int(vp)
        self.height = int(height)
        self.terrain = int(terrain)
        self.object_id = int(obj_id)
        self.contenance = int(contenance)

    def is_inside(self, px, py):
        if self.shape_type == settings.SHAPE_CIRCLE:
            center = Vector2(self.x, self.y)
            return check_collision_point_circle(Vector2(px, py), center, self.dim1)
        elif self.shape_type == settings.SHAPE_RECT:
            rect = Rectangle(self.x - self.dim1 / 2, self.y - self.dim2 / 2, self.dim1, self.dim2)
            return check_collision_point_rec(Vector2(px, py), rect)
        return False


class Edge:
    def __init__(self, uid, id_a, id_b, handle_x, handle_y, type_v, direction, vit, ton, veh, pmax, nat, via, color):
        self.id = uid
        self.node_a = id_a
        self.node_b = id_b
        self.handle_x = int(handle_x)
        self.handle_y = int(handle_y)
        self.type_voie = int(type_v)
        self.direction = int(direction)
        self.vitesse = int(vit)
        self.tonnage = int(ton)
        self.nb_vehicules = int(veh)
        self.poids_max = int(pmax)
        self.nature_sol = int(nat)
        self.viabilite = int(via)
        self.color = color

    def is_handle_inside(self, px, py):
        half = settings.HANDLE_SIZE / 2
        rect = Rectangle(self.handle_x - half, self.handle_y - half, settings.HANDLE_SIZE, settings.HANDLE_SIZE)
        return check_collision_point_rec(Vector2(px, py), rect)


class MapData:
    def __init__(self):
        self.nodes = []
        self.edges = []
        self.next_node_id = 0
        self.next_edge_id = 0

    def add_node(self, x, y, shape_type, dim1, dim2=0, name="", vp=0, height=0, terrain=1, obj_id=1, contenance=0):
        new_node = Node(self.next_node_id, x, y, shape_type, dim1, dim2, name, vp, height, terrain, obj_id, contenance)
        self.nodes.append(new_node)
        self.next_node_id += 1
        return new_node

    def add_edge(self, node_a_obj, node_b_obj, type_v=1, direction=0, vit=0, ton=0, veh=0, pmax=0, nat=0, via=0):
        hx = (node_a_obj.x + node_b_obj.x) // 2
        hy = (node_a_obj.y + node_b_obj.y) // 2
        col = settings.EDGE_TYPE_COLORS.get(type_v, settings.DEFAULT_EDGE_COLOR)
        new_edge = Edge(self.next_edge_id, node_a_obj.id, node_b_obj.id, hx, hy, type_v, direction, vit, ton, veh, pmax,
                        nat, via, col)
        self.edges.append(new_edge)
        self.next_edge_id += 1
        return new_edge

    def delete_node(self, node):
        edges_to_remove = []
        for edge in self.edges:
            if edge.node_a == node.id or edge.node_b == node.id:
                edges_to_remove.append(edge)
        for e in edges_to_remove:
            self.edges.remove(e)
        if node in self.nodes:
            self.nodes.remove(node)

    def delete_edge(self, edge):
        if edge in self.edges:
            self.edges.remove(edge)

    def get_node_at(self, x, y):
        for node in reversed(self.nodes):
            if node.is_inside(x, y):
                return node
        return None

    def get_node_by_id(self, uid):
        for node in self.nodes:
            if node.id == uid: return node
        return None

    def get_edge_handle_at(self, x, y):
        for edge in reversed(self.edges):
            if edge.is_handle_inside(x, y):
                return edge
        return None

    def get_other_node_id(self, edge, start_node_id):
        if edge.node_a == start_node_id: return edge.node_b
        if edge.node_b == start_node_id: return edge.node_a
        return None

    def draw_editor(self, show_nodes=True, show_edges=True, show_handles=True):
        for edge in self.edges:
            node_a = self.get_node_by_id(edge.node_a)
            node_b = self.get_node_by_id(edge.node_b)
            if node_a and node_b:
                if show_edges:
                    draw_line_ex(Vector2(node_a.x, node_a.y), Vector2(edge.handle_x, edge.handle_y), 3, edge.color)
                    draw_line_ex(Vector2(edge.handle_x, edge.handle_y), Vector2(node_b.x, node_b.y), 3, edge.color)
                if show_handles:
                    hs = settings.HANDLE_SIZE
                    draw_rectangle(edge.handle_x - int(hs / 2), edge.handle_y - int(hs / 2), hs, hs, edge.color)
                    draw_rectangle_lines(edge.handle_x - int(hs / 2), edge.handle_y - int(hs / 2), hs, hs, WHITE)

        if show_nodes:
            for node in self.nodes:
                color = settings.NODE_EDIT_COLOR
                if node.shape_type == settings.SHAPE_CIRCLE:
                    draw_circle_lines(node.x, node.y, node.dim1, color)
                elif node.shape_type == settings.SHAPE_RECT:
                    left = int(node.x - (node.dim1 / 2))
                    top = int(node.y - (node.dim2 / 2))
                    draw_rectangle_lines(left, top, node.dim1, node.dim2, color)
                id_str = str(node.id)
                text_w = measure_text(id_str, 20)
                draw_text(id_str, node.x - int(text_w / 2), node.y - 10, 20, BLACK)

    def draw_single_edge_lines(self, edge):
        node_a = self.get_node_by_id(edge.node_a)
        node_b = self.get_node_by_id(edge.node_b)
        if node_a and node_b:
            draw_line_ex(Vector2(node_a.x, node_a.y), Vector2(edge.handle_x, edge.handle_y), 3, edge.color)
            draw_line_ex(Vector2(edge.handle_x, edge.handle_y), Vector2(node_b.x, node_b.y), 3, edge.color)

    def draw_test_path(self, path_edges):
        """Dessine le chemin manuel en vert"""
        for edge in path_edges:
            node_a = self.get_node_by_id(edge.node_a)
            node_b = self.get_node_by_id(edge.node_b)
            if node_a and node_b:
                draw_line_ex(Vector2(node_a.x, node_a.y), Vector2(edge.handle_x, edge.handle_y), 5, GREEN)
                draw_line_ex(Vector2(edge.handle_x, edge.handle_y), Vector2(node_b.x, node_b.y), 5, GREEN)

    # --- ALGORITHME DIJKSTRA ---
    def find_shortest_path(self, start_id, end_id, vehicle_weight):
        """
        Retourne une liste d'Edges ordonnée du départ à l'arrivée.
        Prend en compte : Poids Max et Direction.
        Coût = edge.vitesse
        """
        # 1. Init
        distances = {node.id: float('inf') for node in self.nodes}
        distances[start_id] = 0
        previous = {node.id: None for node in self.nodes}  # Stocke (from_node_id, edge_used)

        # File de priorité : (cout_total, node_id)
        pq = [(0, start_id)]

        while pq:
            current_cost, current_id = heapq.heappop(pq)

            if current_id == end_id:
                break  # On a trouvé

            if current_cost > distances[current_id]:
                continue

            # Trouver les voisins via les Edges
            for edge in self.edges:
                # Filtre Poids
                if vehicle_weight > edge.poids_max:
                    continue

                neighbor_id = None

                # Filtre Direction et Connexion
                # Si Dir=0 (Double) ou Dir=1 (A->B)
                if edge.node_a == current_id and (edge.direction == 0 or edge.direction == 1):
                    neighbor_id = edge.node_b

                # Si Dir=0 (Double) ou Dir=2 (B->A)
                elif edge.node_b == current_id and (edge.direction == 0 or edge.direction == 2):
                    neighbor_id = edge.node_a

                if neighbor_id is not None:
                    # Le coût est la "Vitesse" de la voie (comme défini dans tes specs)
                    new_cost = current_cost + edge.vitesse

                    if new_cost < distances[neighbor_id]:
                        distances[neighbor_id] = new_cost
                        previous[neighbor_id] = (current_id, edge)
                        heapq.heappush(pq, (new_cost, neighbor_id))

        # 2. Reconstruction du chemin (Backtracking)
        path_edges = []
        curr = end_id
        while curr != start_id:
            prev_info = previous.get(curr)
            if prev_info is None:
                return None  # Pas de chemin trouvé

            prev_node, edge_used = prev_info
            path_edges.append(edge_used)
            curr = prev_node

        path_edges.reverse()  # On remet dans l'ordre Départ -> Arrivée
        return path_edges

    def draw_auto_path(self, path_edges, autonomy):
        """Dessine le chemin Rouge (Théorique) puis Vert (Possible)"""
        current_cost = 0

        for edge in path_edges:
            node_a = self.get_node_by_id(edge.node_a)
            node_b = self.get_node_by_id(edge.node_b)

            if node_a and node_b:
                # On détermine la couleur
                # Si le coût accumulé AVEC cette route dépasse l'autonomie, on passe en ROUGE
                # Sinon on est en VERT
                cost_after_move = current_cost + edge.vitesse

                # 1. DESSIN ROUGE (Fond) - Toujours dessiné
                draw_line_ex(Vector2(node_a.x, node_a.y), Vector2(edge.handle_x, edge.handle_y), 6, RED)
                draw_line_ex(Vector2(edge.handle_x, edge.handle_y), Vector2(node_b.x, node_b.y), 6, RED)

                # 2. DESSIN VERT (Dessus) - Si on a du jus
                if cost_after_move <= autonomy:
                    draw_line_ex(Vector2(node_a.x, node_a.y), Vector2(edge.handle_x, edge.handle_y), 4, GREEN)
                    draw_line_ex(Vector2(edge.handle_x, edge.handle_y), Vector2(node_b.x, node_b.y), 4, GREEN)

                current_cost = cost_after_move

    def reset(self):
        self.nodes = []
        self.edges = []
        self.next_node_id = 0
        self.next_edge_id = 0