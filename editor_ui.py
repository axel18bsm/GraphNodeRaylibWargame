from pyray import *
from pyray import ffi
import settings


class EditorUI:
    def __init__(self):
        # ---------------------------------------------------------
        # 1. CALCULS DE MISE EN PAGE (LAYOUT)
        # ---------------------------------------------------------
        self.panel_x = settings.SCREEN_WIDTH - settings.PANEL_RIGHT_WIDTH
        self.ui_padding = 10
        self.total_width = settings.PANEL_RIGHT_WIDTH - (2 * self.ui_padding)
        self.ui_start_x = self.panel_x + self.ui_padding
        self.control_height = 30

        # --- EN-TETE GLOBAL ---
        self.rect_mode_group = Rectangle(self.ui_start_x, 45, self.total_width / 2, self.control_height)

        # --- ZONE EDITION : BOUTONS FICHIERS ---
        self.btn_sub_width = (self.total_width - 10) / 3
        y_line_2 = 85
        self.rect_btn_nouveau = Rectangle(self.ui_start_x, y_line_2, self.btn_sub_width, self.control_height)
        self.rect_btn_ouvrir = Rectangle(self.ui_start_x + self.btn_sub_width + 5, y_line_2, self.btn_sub_width,
                                         self.control_height)
        self.rect_btn_save = Rectangle(self.ui_start_x + (self.btn_sub_width + 5) * 2, y_line_2, self.btn_sub_width,
                                       self.control_height)

        # CORRECTION : Définition de la hauteur de contenu (manquante précédemment)
        self.start_y_content = 130

        # --- ZONE EDITION : OUTILS ---
        self.rect_tool_group = Rectangle(self.ui_start_x, 160, self.total_width / 3, self.control_height)

        # Filtres d'affichage (Checkboxes)
        y_filters = 200
        chk_width = 20
        col_width = self.total_width / 3
        self.rect_chk_nodes = Rectangle(self.ui_start_x, y_filters, chk_width, chk_width)
        self.rect_chk_edges = Rectangle(self.ui_start_x + col_width, y_filters, chk_width, chk_width)
        self.rect_chk_handles = Rectangle(self.ui_start_x + col_width * 2, y_filters, chk_width, chk_width)

        self.show_nodes_ptr = ffi.new("bool *", True)
        self.show_edges_ptr = ffi.new("bool *", True)
        self.show_handles_ptr = ffi.new("bool *", True)

        # --- ZONE EDITION : PROPRIETES (Position de départ) ---
        self.start_y_props = 240
        current_y = self.start_y_props

        # -- Props NOEUD --
        self.rect_shape_group = Rectangle(self.ui_start_x, current_y + 25, (self.total_width / 2) / 2,
                                          self.control_height)
        self.rect_radius_val = Rectangle(self.ui_start_x + 80, current_y + 65, 80, 25)
        self.rect_width_val = Rectangle(self.ui_start_x + 80, current_y + 65, 80, 25)
        self.rect_height_val = Rectangle(self.ui_start_x + 80, current_y + 95, 80, 25)
        current_y += 135

        self.rect_label_name = Rectangle(self.ui_start_x, current_y, 50, 25)
        self.rect_input_name = Rectangle(self.ui_start_x + 60, current_y, 180, 25)
        current_y += 35
        self.rect_label_vp = Rectangle(self.ui_start_x, current_y, 50, 25)
        self.rect_input_vp = Rectangle(self.ui_start_x + 60, current_y, 100, 25)
        current_y += 35
        self.rect_label_height = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_input_height = Rectangle(self.ui_start_x + 70, current_y, 120, 25)
        current_y += 35
        self.rect_label_terrain = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_input_terrain = Rectangle(self.ui_start_x + 70, current_y, 120, 25)
        current_y += 35
        self.rect_label_obj = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_input_obj = Rectangle(self.ui_start_x + 70, current_y, 120, 25)
        current_y += 35
        self.rect_label_cap = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_input_cap = Rectangle(self.ui_start_x + 70, current_y, 120, 25)

        # -- Props VOIE --
        current_y = self.start_y_props
        self.rect_edge_type_lbl = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_edge_type_val = Rectangle(self.ui_start_x + 70, current_y, 120, 25)
        current_y += 35
        self.rect_edge_dir_lbl = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_edge_dir_val = Rectangle(self.ui_start_x + 70, current_y, 120, 25)
        current_y += 35
        self.rect_edge_vit_lbl = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_edge_vit_val = Rectangle(self.ui_start_x + 70, current_y, 120, 25)
        current_y += 35
        self.rect_edge_ton_lbl = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_edge_ton_val = Rectangle(self.ui_start_x + 70, current_y, 120, 25)
        current_y += 35
        self.rect_edge_veh_lbl = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_edge_veh_val = Rectangle(self.ui_start_x + 70, current_y, 120, 25)
        current_y += 35
        self.rect_edge_pmax_lbl = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_edge_pmax_val = Rectangle(self.ui_start_x + 70, current_y, 120, 25)
        current_y += 35
        self.rect_edge_nat_lbl = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_edge_nat_val = Rectangle(self.ui_start_x + 70, current_y, 120, 25)
        current_y += 35
        self.rect_edge_via_lbl = Rectangle(self.ui_start_x, current_y, 60, 25)
        self.rect_edge_via_val = Rectangle(self.ui_start_x + 70, current_y, 120, 25)

        # --- ZONE TEST : BOUTONS SOUS-MODES ---
        test_btn_w = (self.total_width - 5) / 3
        self.rect_test_submode = Rectangle(self.ui_start_x, y_line_2, test_btn_w, self.control_height)

        # --- ZONE TEST : PARAMETRES SIMULATION ---
        sim_y = 130
        self.rect_sim_weight_lbl = Rectangle(self.ui_start_x, sim_y, 100, 25)
        self.rect_sim_weight_val = Rectangle(self.ui_start_x + 110, sim_y, 80, 25)
        sim_y += 35
        self.rect_sim_auto_lbl = Rectangle(self.ui_start_x, sim_y, 100, 25)
        self.rect_sim_auto_val = Rectangle(self.ui_start_x + 110, sim_y, 80, 25)
        sim_y += 45
        self.rect_btn_reset_sim = Rectangle(self.ui_start_x, sim_y, self.total_width, 30)

        # ---------------------------------------------------------
        # 2. POINTEURS C (FFI) POUR RAYGUI
        # ---------------------------------------------------------

        # Node Pointers
        self.selected_shape = ffi.new("int *", settings.SHAPE_CIRCLE)
        self.radius_ptr = ffi.new("int *", 50)
        self.width_ptr = ffi.new("int *", 100)
        self.height_ptr = ffi.new("int *", 50)
        self.name_buffer = ffi.new("char[64]")
        self.edit_name_mode = False
        self.vp_ptr = ffi.new("int *", 0)
        self.edit_vp_mode = False
        self.height_val_ptr = ffi.new("int *", 0)
        self.edit_height_mode = False
        self.terrain_val_ptr = ffi.new("int *", 1)
        self.edit_terrain_mode = False
        self.obj_val_ptr = ffi.new("int *", 1)
        self.edit_obj_mode = False
        self.cap_ptr = ffi.new("int *", 0)
        self.edit_cap_mode = False
        self.edit_radius = False
        self.edit_width = False
        self.edit_height = False

        # Edge Pointers
        self.edge_type_ptr = ffi.new("int *", 1)
        self.edge_edit_type = False
        self.edge_dir_ptr = ffi.new("int *", 0)
        self.edge_edit_dir = False
        self.edge_vit_ptr = ffi.new("int *", 0)
        self.edge_edit_vit = False
        self.edge_ton_ptr = ffi.new("int *", 0)
        self.edge_edit_ton = False
        self.edge_veh_ptr = ffi.new("int *", 0)
        self.edge_edit_veh = False
        self.edge_pmax_ptr = ffi.new("int *", 0)
        self.edge_edit_pmax = False
        self.edge_nat_ptr = ffi.new("int *", 0)
        self.edge_edit_nat = False
        self.edge_via_ptr = ffi.new("int *", 0)
        self.edge_edit_via = False

        # Simulation Pointers
        self.sim_weight_ptr = ffi.new("int *", 10)
        self.sim_weight_edit = False
        self.sim_auto_ptr = ffi.new("int *", 100)
        self.sim_auto_edit = False

        # Popup
        list_w = 400
        list_h = 300
        self.rect_list_view = Rectangle((settings.VIEW_WIDTH - list_w) / 2, (settings.VIEW_HEIGHT - list_h) / 2, list_w,
                                        list_h)
        self.rect_close_list = Rectangle(self.rect_list_view.x + list_w - 30, self.rect_list_view.y - 30, 30, 30)
        self.scroll_index = ffi.new("int *", 0)
        self.active_index = ffi.new("int *", -1)
        self.show_list = False
        self.popup_mode = None
        self.rect_message_box = Rectangle((settings.VIEW_WIDTH - 400) / 2, (settings.VIEW_HEIGHT - 150) / 2, 400, 150)

    # ---------------------------------------------------------
    # HELPERS
    # ---------------------------------------------------------
    def get_name_str(self):
        return ffi.string(self.name_buffer).decode('utf-8')

    def set_name_str(self, text):
        encoded = text.encode('utf-8')
        ffi.memmove(self.name_buffer, b'\0' * 64, 64)
        limit = min(len(encoded), 63)
        ffi.memmove(self.name_buffer, encoded, limit)

    def is_name_being_edited(self):
        return self.edit_name_mode

    # ---------------------------------------------------------
    # DRAW LOOP
    # ---------------------------------------------------------
    def draw(self, current_mode, test_submode, current_tool, log_message, map_engine, file_list_str=None,
             project_list_str=None, sim_edge_info=None, inspection_info_text=None):
        selected_index = None
        selection_type = None
        save_requested = False
        new_test_submode = test_submode
        reset_sim_requested = False

        draw_rectangle(int(self.panel_x), 0, settings.PANEL_RIGHT_WIDTH, settings.SCREEN_HEIGHT, settings.UI_BG_COLOR)
        draw_rectangle_lines(int(self.panel_x), 0, settings.PANEL_RIGHT_WIDTH, settings.SCREEN_HEIGHT,
                             settings.UI_BORDER_COLOR)
        draw_rectangle(0, settings.SCREEN_HEIGHT - settings.PANEL_BOTTOM_HEIGHT, settings.VIEW_WIDTH,
                       settings.PANEL_BOTTOM_HEIGHT, settings.UI_BG_COLOR)
        draw_rectangle_lines(0, settings.SCREEN_HEIGHT - settings.PANEL_BOTTOM_HEIGHT, settings.VIEW_WIDTH,
                             settings.PANEL_BOTTOM_HEIGHT, settings.UI_BORDER_COLOR)

        draw_text("COMMANDES", int(self.panel_x) + 10, 10, 20, settings.TEXT_COLOR)
        draw_text("LOGS", 10, settings.SCREEN_HEIGHT - settings.PANEL_BOTTOM_HEIGHT + 10, 20, settings.TEXT_COLOR)

        mode_ptr = ffi.new("int *", current_mode)
        gui_toggle_group(self.rect_mode_group, "EDITION;TEST", mode_ptr)
        if mode_ptr[0] != current_mode:
            current_mode = mode_ptr[0]
            log_message = "Mode Edition." if current_mode == settings.MODE_EDITION else "Mode Test."

        # =========================================================
        # MODE EDITION
        # =========================================================
        if current_mode == settings.MODE_EDITION:
            if gui_button(self.rect_btn_nouveau, "NOUVEAU"):
                self.show_list = True
                self.popup_mode = "FILE"
                self.active_index[0] = -1
            if gui_button(self.rect_btn_ouvrir, "OUVRIR"):
                self.show_list = True
                self.popup_mode = "PROJECT"
                self.active_index[0] = -1
            if gui_button(self.rect_btn_save, "SAUVE"):
                save_requested = True

            nom_carte = map_engine.current_map_name if map_engine.current_map_name else "Aucune"
            if len(nom_carte) > 25: nom_carte = "..." + nom_carte[-22:]
            gui_label(Rectangle(self.ui_start_x, self.start_y_content, self.total_width, 20), f"Outils : {nom_carte}")

            active_tool_ptr = ffi.new("int *", current_tool)
            gui_toggle_group(self.rect_tool_group, "VILLE;VOIES;SUPPR", active_tool_ptr)
            if active_tool_ptr[0] != current_tool:
                current_tool = active_tool_ptr[0]
                log_message = "Changement Outil..."

            gui_check_box(self.rect_chk_nodes, "Noeuds", self.show_nodes_ptr)
            gui_check_box(self.rect_chk_edges, "Voies", self.show_edges_ptr)
            gui_check_box(self.rect_chk_handles, "Poign.", self.show_handles_ptr)

            if current_tool == settings.TOOL_NODE:
                gui_label(Rectangle(self.ui_start_x, self.start_y_props, self.total_width, 20), "Proprietes Noeud:")
                gui_toggle_group(self.rect_shape_group, "ROND;RECT", self.selected_shape)
                if self.selected_shape[0] == settings.SHAPE_CIRCLE:
                    draw_text("Rayon:", int(self.ui_start_x), int(self.rect_radius_val.y) + 5, 20, WHITE)
                    if gui_value_box(self.rect_radius_val, "", self.radius_ptr, 1, 1000, self.edit_radius):
                        self.edit_radius = not self.edit_radius
                else:
                    draw_text("Larg:", int(self.ui_start_x), int(self.rect_width_val.y) + 5, 20, WHITE)
                    if gui_value_box(self.rect_width_val, "", self.width_ptr, 1, 1000, self.edit_width):
                        self.edit_width = not self.edit_width
                    draw_text("Haut:", int(self.ui_start_x), int(self.rect_height_val.y) + 5, 20, WHITE)
                    if gui_value_box(self.rect_height_val, "", self.height_ptr, 1, 1000, self.edit_height):
                        self.edit_height = not self.edit_height
                draw_text("Nom:", int(self.rect_label_name.x), int(self.rect_label_name.y) + 5, 20, WHITE)
                if gui_text_box(self.rect_input_name, self.name_buffer, 64, self.edit_name_mode):
                    self.edit_name_mode = not self.edit_name_mode
                draw_text("PV:", int(self.rect_label_vp.x), int(self.rect_label_vp.y) + 5, 20, WHITE)
                if gui_value_box(self.rect_input_vp, "", self.vp_ptr, 0, 999, self.edit_vp_mode):
                    self.edit_vp_mode = not self.edit_vp_mode
                draw_text("Haut:", int(self.rect_label_height.x), int(self.rect_label_height.y) + 5, 20, WHITE)
                if gui_spinner(self.rect_input_height, "", self.height_val_ptr, -4, 6, self.edit_height_mode):
                    self.edit_height_mode = not self.edit_height_mode
                draw_text("Terr:", int(self.rect_label_terrain.x), int(self.rect_label_terrain.y) + 5, 20, WHITE)
                if gui_spinner(self.rect_input_terrain, "", self.terrain_val_ptr, 1, 15, self.edit_terrain_mode):
                    self.edit_terrain_mode = not self.edit_terrain_mode
                draw_text("Obj:", int(self.rect_label_obj.x), int(self.rect_label_obj.y) + 5, 20, WHITE)
                if gui_spinner(self.rect_input_obj, "", self.obj_val_ptr, 1, 15, self.edit_obj_mode):
                    self.edit_obj_mode = not self.edit_obj_mode
                draw_text("Cont:", int(self.rect_label_cap.x), int(self.rect_label_cap.y) + 5, 20, WHITE)
                if gui_value_box(self.rect_input_cap, "", self.cap_ptr, 0, 9999, self.edit_cap_mode):
                    self.edit_cap_mode = not self.edit_cap_mode

            elif current_tool == settings.TOOL_EDGE:
                gui_label(Rectangle(self.ui_start_x, self.start_y_props, self.total_width, 20), "Proprietes Voie:")
                draw_text("Type:", int(self.rect_edge_type_lbl.x), int(self.rect_edge_type_lbl.y) + 5, 20, WHITE)
                if gui_spinner(self.rect_edge_type_val, "", self.edge_type_ptr, 1, 15, self.edge_edit_type):
                    self.edge_edit_type = not self.edge_edit_type
                draw_text("Dir:", int(self.rect_edge_dir_lbl.x), int(self.rect_edge_dir_lbl.y) + 5, 20, WHITE)
                if gui_spinner(self.rect_edge_dir_val, "", self.edge_dir_ptr, 0, 2, self.edge_edit_dir):
                    self.edge_edit_dir = not self.edge_edit_dir
                draw_text("Cout:", int(self.rect_edge_vit_lbl.x), int(self.rect_edge_vit_lbl.y) + 5, 20, WHITE)
                if gui_value_box(self.rect_edge_vit_val, "", self.edge_vit_ptr, 0, 999, self.edge_edit_vit):
                    self.edge_edit_vit = not self.edge_edit_vit
                draw_text("Ton:", int(self.rect_edge_ton_lbl.x), int(self.rect_edge_ton_lbl.y) + 5, 20, WHITE)
                if gui_value_box(self.rect_edge_ton_val, "", self.edge_ton_ptr, 0, 999, self.edge_edit_ton):
                    self.edge_edit_ton = not self.edge_edit_ton
                draw_text("Veh:", int(self.rect_edge_veh_lbl.x), int(self.rect_edge_veh_lbl.y) + 5, 20, WHITE)
                if gui_value_box(self.rect_edge_veh_val, "", self.edge_veh_ptr, 0, 999, self.edge_edit_veh):
                    self.edge_edit_veh = not self.edge_edit_veh
                draw_text("P.Max:", int(self.rect_edge_pmax_lbl.x), int(self.rect_edge_pmax_lbl.y) + 5, 20, WHITE)
                if gui_value_box(self.rect_edge_pmax_val, "", self.edge_pmax_ptr, 0, 999, self.edge_edit_pmax):
                    self.edge_edit_pmax = not self.edge_edit_pmax
                draw_text("Nat:", int(self.rect_edge_nat_lbl.x), int(self.rect_edge_nat_lbl.y) + 5, 20, WHITE)
                if gui_spinner(self.rect_edge_nat_val, "", self.edge_nat_ptr, 0, 10, self.edge_edit_nat):
                    self.edge_edit_nat = not self.edge_edit_nat
                draw_text("Via:", int(self.rect_edge_via_lbl.x), int(self.rect_edge_via_lbl.y) + 5, 20, WHITE)
                if gui_spinner(self.rect_edge_via_val, "", self.edge_via_ptr, 0, 5, self.edge_edit_via):
                    self.edge_edit_via = not self.edge_edit_via

        # =========================================================
        # MODE TEST (SIMULATION)
        # =========================================================
        elif current_mode == settings.MODE_TEST:

            test_mode_ptr = ffi.new("int *", test_submode)
            gui_toggle_group(self.rect_test_submode, "INSPEC;MANUEL;AUTO", test_mode_ptr)
            new_test_submode = test_mode_ptr[0]

            if new_test_submode == settings.TEST_SUB_INSPEC:
                gui_label(Rectangle(self.ui_start_x, 130, self.total_width, 20), "INFORMATIONS:")
                if inspection_info_text:
                    self.draw_inspection_info(inspection_info_text)

            else:
                titre = "SIMU. MANUELLE" if new_test_submode == settings.TEST_SUB_MANUAL else "CALCUL AUTO"
                draw_text(titre, int(self.ui_start_x), 130, 20, WHITE)

                # Parametres Véhicule
                draw_text("Poids Vehicule:", int(self.rect_sim_weight_lbl.x), int(self.rect_sim_weight_lbl.y) + 5, 20,
                          WHITE)
                if gui_value_box(self.rect_sim_weight_val, "", self.sim_weight_ptr, 0, 1000, self.sim_weight_edit):
                    self.sim_weight_edit = not self.sim_weight_edit

                draw_text("Autonomie Max:", int(self.rect_sim_auto_lbl.x), int(self.rect_sim_auto_lbl.y) + 5, 20, WHITE)
                if gui_value_box(self.rect_sim_auto_val, "", self.sim_auto_ptr, 1, 9999, self.sim_auto_edit):
                    self.sim_auto_edit = not self.sim_auto_edit

                if gui_button(self.rect_btn_reset_sim, "RESET TRAJET"):
                    reset_sim_requested = True

                if new_test_submode == settings.TEST_SUB_MANUAL and sim_edge_info:
                    info_y = self.rect_btn_reset_sim.y + 45
                    gui_label(Rectangle(self.ui_start_x, info_y, self.total_width, 20), "INFO VOIE:")
                    self.draw_inspection_info(sim_edge_info, start_y=info_y + 25)

                if new_test_submode == settings.TEST_SUB_AUTO:
                    draw_text("1. Clic Depart (Vert)", int(self.ui_start_x), int(self.rect_btn_reset_sim.y) + 40, 18,
                              GRAY)
                    draw_text("2. Clic Arrivee (Rouge)", int(self.ui_start_x), int(self.rect_btn_reset_sim.y) + 60, 18,
                              GRAY)

        # Footer
        draw_text(log_message, 20, settings.SCREEN_HEIGHT - 100, 20, settings.LOG_COLOR)
        mouse_world = map_engine.get_mouse_world_pos()
        coord_text = f"X: {int(mouse_world.x)}  Y: {int(mouse_world.y)}"
        draw_text(coord_text, 20, settings.SCREEN_HEIGHT - 40, 20, settings.COORD_COLOR)

        if self.show_list and (file_list_str or project_list_str):
            titre = "CHOIX"
            contenu = file_list_str if self.popup_mode == "FILE" else project_list_str
            if contenu:
                draw_rectangle(0, 0, settings.VIEW_WIDTH, settings.VIEW_HEIGHT, fade(BLACK, 0.7))
                draw_text(titre, int(self.rect_list_view.x), int(self.rect_list_view.y) - 25, 20, WHITE)
                if gui_button(self.rect_close_list, "X"): self.close_selector()
                gui_list_view(self.rect_list_view, contenu, self.scroll_index, self.active_index)
                if self.active_index[0] >= 0:
                    selected_index = self.active_index[0]
                    selection_type = self.popup_mode

        return current_mode, new_test_submode, current_tool, log_message, selected_index, selection_type, save_requested, reset_sim_requested

    def close_selector(self):
        self.show_list = False
        self.popup_mode = None
        self.active_index[0] = -1

    def sync_ui_with_node(self, node):
        self.selected_shape[0] = node.shape_type
        if node.shape_type == settings.SHAPE_CIRCLE:
            self.radius_ptr[0] = node.dim1
        else:
            self.width_ptr[0] = node.dim1
            self.height_ptr[0] = node.dim2
        self.set_name_str(node.name)
        self.edit_name_mode = False
        self.vp_ptr[0] = node.vp
        self.height_val_ptr[0] = node.height
        self.terrain_val_ptr[0] = node.terrain
        self.obj_val_ptr[0] = node.object_id
        self.cap_ptr[0] = node.contenance

    def sync_ui_with_edge(self, edge):
        self.edge_type_ptr[0] = edge.type_voie
        self.edge_dir_ptr[0] = edge.direction
        self.edge_vit_ptr[0] = edge.vitesse
        self.edge_ton_ptr[0] = edge.tonnage
        self.edge_veh_ptr[0] = edge.nb_vehicules
        self.edge_pmax_ptr[0] = edge.poids_max
        self.edge_nat_ptr[0] = edge.nature_sol
        self.edge_via_ptr[0] = edge.viabilite

    def draw_confirmation_popup(self, message):
        result = gui_message_box(self.rect_message_box, "CONFIRMATION", message, "Oui;Non")
        return result

    def draw_inspection_info(self, info_text, start_y=160):
        lines = info_text.split('\n')
        y = start_y
        for line in lines:
            draw_text(line, int(self.ui_start_x), int(y), 18, WHITE)
            y += 25