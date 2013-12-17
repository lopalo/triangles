from kivy.app import App
from kivy.lang import Builder
from kivy.uix.widget import Widget
from kivy.uix.boxlayout import BoxLayout
from kivy.properties import (
    ObjectProperty, NumericProperty,
    ReferenceListProperty)
from kivy.vector import Vector

from world import World


class StartMenu(BoxLayout):

    def do_connect(self, *args, **kwargs):
        self.parent.do_connect(*args, **kwargs)


class LeftControl(Widget):
    outer_rad = NumericProperty(0)
    inner_x = NumericProperty(0)
    inner_y = NumericProperty(0)
    inner_pos = ReferenceListProperty(inner_x, inner_y)
    inner_rad = NumericProperty(0)
    pressed = False


    def on_touch_down(self, touch):
        center_vect = Vector(self.center)
        vect = Vector(touch.pos) - center_vect
        if vect.length() > self.outer_rad:
            return
        self.pressed = True
        self.inner_pos = vect + center_vect
        self.move_vector(vect)

    def on_touch_move(self, touch):
        if not self.pressed:
            return
        center_vect = Vector(self.center)
        vect = Vector(touch.pos) - center_vect
        if vect.length() > self.outer_rad:
            vect /= vect.length() / self.outer_rad
        self.inner_pos = vect + center_vect
        self.move_vector(vect)

    def on_touch_up(self, touch):
        if not self.pressed:
            return
        self.pressed = False
        self.inner_pos = self.center
        self.move_vector(Vector(0, 0))

    def move_vector(self, vect):
        length = vect.length() / self.outer_rad
        World.move_vector(length, vect.angle(Vector(1, 0)))


class RightControl(Widget):
    pressed = False

    def on_touch_down(self, touch):
        vect = Vector(touch.pos) - Vector(self.center)
        if vect.length() > self.radius:
            return
        self.pressed = True
        World.fire(True)

    def on_touch_up(self, touch):
        if not self.pressed:
            return
        self.pressed = False
        World.fire(False)


class UI(Widget):
    pass


class GameWidget(Widget):

    world = ObjectProperty(None)
    ui = ObjectProperty(None)


class MainWidget(Widget):

    start_menu = ObjectProperty(None)
    game_widget = ObjectProperty(None)

    def do_connect(self, address, name):
        self.remove_widget(self.start_menu)
        self.game_widget = GameWidget()
        self.add_widget(self.game_widget)


class GameApp(App):

    def build(self):
        #TODO: remove
        #self.root.do_connect('localhost:7777', 'anonymous')
        return self.root



if __name__ == '__main__':
    GameApp().run()
