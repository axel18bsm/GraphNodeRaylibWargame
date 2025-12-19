import os
from pyray import *
import settings


class MapEngine:
    def __init__(self):
        self.texture = None
        self.current_map_name = None
        self.camera = Camera2D()
        self.camera.target = Vector2(0, 0)
        self.camera.offset = Vector2(0, 0)
        self.camera.rotation = 0.0
        self.camera.zoom = 1.0
        self.total_motion = 0.0
        self.was_clicked = False
        self.load_map(settings.IMAGE_FILENAME)

    def _load_texture_from_disk(self, full_path):
        if self.texture:
            unload_texture(self.texture)
        if os.path.exists(full_path):
            image = load_image(full_path)
            self.texture = load_texture_from_image(image)
            unload_image(image)
        else:
            img = gen_image_checked(2000, 2000, 50, 50, LIGHTGRAY, GRAY)
            self.texture = load_texture_from_image(img)
            unload_image(img)

    def load_map(self, filename):
        self.current_map_name = filename
        full_path = os.path.join(settings.RESOURCE_DIR, filename)
        self._load_texture_from_disk(full_path)
        self.camera.target = Vector2(0, 0)

    def load_project(self, project_name):
        image_name = project_name + ".png"
        self.current_map_name = image_name
        full_path = os.path.join(settings.SAVED_DIR, project_name, image_name)
        self._load_texture_from_disk(full_path)
        self.camera.target = Vector2(0, 0)

    def update(self, locked=False):
        self.was_clicked = False
        if locked: return

        delta = get_mouse_delta()

        if is_mouse_button_pressed(MOUSE_BUTTON_LEFT):
            self.total_motion = 0.0

        if is_mouse_button_down(MOUSE_BUTTON_LEFT):
            self.camera.target.x -= delta.x
            self.camera.target.y -= delta.y
            self.total_motion += abs(delta.x) + abs(delta.y)

        if is_mouse_button_released(MOUSE_BUTTON_LEFT):
            if self.total_motion < 6.0:
                self.was_clicked = True

        if self.texture:
            max_target_x = max(0, self.texture.width - settings.VIEW_WIDTH)
            max_target_y = max(0, self.texture.height - settings.VIEW_HEIGHT)
            self.camera.target.x = clamp(self.camera.target.x, 0, max_target_x)
            self.camera.target.y = clamp(self.camera.target.y, 0, max_target_y)

    def draw(self):
        if self.texture:
            begin_mode_2d(self.camera)
            draw_texture(self.texture, 0, 0, WHITE)
            draw_rectangle_lines(0, 0, self.texture.width, self.texture.height, RED)
            end_mode_2d()

    def get_mouse_world_pos(self):
        return get_screen_to_world_2d(get_mouse_position(), self.camera)

    def unload(self):
        if self.texture:
            unload_texture(self.texture)