import os
import json
import logging

from time import time
from kivy.app import App
from kivy.core.window import Window
from kivy.uix.widget import Widget
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.label import Label
from kivy.clock import Clock
from kivy.properties import (
    ObjectProperty, NumericProperty,
    ReferenceListProperty)
from kivy.vector import Vector
from kivy.logger import Logger

from world import World
from controller import Controller

PING_CHECK_PERIO = 1 # seconds


class StartMenu(BoxLayout):

    def connect(self, *args, **kwargs):
        self.parent.connect(*args, **kwargs)


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


class Scores(BoxLayout):
    records = NumericProperty(10)

    def __init__(self, *args, **kwargs):
        super(Scores, self).__init__(*args, **kwargs)
        self._records = []
        for _ in range(self.records):
            record = Label(text='')
            self.add_widget(record)
            self._records.append(record)

    def handle_update(self, scores):
        if not scores: #NOTE: can be an empty list
            return
        score_list = sorted(scores.items(), key=lambda i: i[1],
                                    reverse=True)[:self.records]
        for widget in self._records:
            widget.text = ''
        for (name, score), widget in zip(score_list, self._records):
            widget.text = "{}: {}".format(name, score)

class Ping(Label):
    _timestamp = None
    ping = NumericProperty(0)

    def send_ping(self, dt=None):
        timestamp = int(time() * 1000)
        Controller.send(cmd="ping", args=dict(timestamp=timestamp))

    def handle(self, timestamp):
        self.ping = int(time() * 1000)  - timestamp
        Clock.schedule_once(self.send_ping, PING_CHECK_PERIO)


class UI(Widget):
    pass


class GameWidget(Widget):

    ui = ObjectProperty(None)

    def initialize(self, *args, **kwargs):
        World.activate(self, *args, **kwargs)
        Controller.add_handler('scores', self.ui.scores)
        Controller.add_handler('ping', self.ui.ping)
        self.ui.ping.send_ping()


class MainWidget(Widget):

    start_menu = ObjectProperty(None)
    game_widget = ObjectProperty(None)

    def connect(self, address, name, fake_ping):
        self.remove_widget(self.start_menu)
        Controller.set_fake_ping(int(fake_ping))
        Controller.connect(address, wait=True)
        self.game_widget = GameWidget()
        self.game_widget.initialize(name)
        self.add_widget(self.game_widget)


class KeyBoard(object):

    def __init__(self):
        keyboard = Window.request_keyboard(None, self)
        keyboard.bind(on_key_down=self.on_key_down,
                      on_key_up=self.on_key_up)
        self._direction = Vector(0, 0)

    def on_key_down(self, keyboard, (code, kname), text, mod):
        if kname == 'spacebar':
            World.fire(True)
        if kname == 'right':
            self._direction[0] = 1
        if kname == 'left':
            self._direction[0] = -1
        if kname == 'up':
            self._direction[1] = 1
        if kname == 'down':
            self._direction[1] = -1
        self._update_move_vector()

    def on_key_up(self, keyboard, (code, kname)):
        if kname == 'spacebar':
            World.fire(False)
        if kname == 'right':
            self._direction[0] = 0
        if kname == 'left':
            self._direction[0] = 0
        if kname == 'up':
            self._direction[1] = 0
        if kname == 'down':
            self._direction[1] = 0
        self._update_move_vector()

    def _update_move_vector(self):
        direct = self._direction
        length, angle = direct.length(), direct.angle(Vector(1, 0))
        World.move_vector(min(length, 1), angle)



class GameApp(App):

    def __init__(self, start_args=None, **kwargs):
        self.start_args = start_args
        super(GameApp, self).__init__(**kwargs)

    def build(self):
        Controller.activate()
        start_args = self.start_args
        if start_args is not None:
            self.root.connect(start_args['address'], start_args['name'])
        return self.root



if __name__ == '__main__':
    Logger.setLevel(logging.INFO)
    start_args = None
    if 'TRI_START_ARGS' in os.environ:
        with open(os.environ['TRI_START_ARGS']) as f:
            start_args = json.load(f)
    KeyBoard()
    GameApp(start_args=start_args).run()


