from kivy.clock import Clock
from kivy.logger import Logger as log

from network import FakeConnection

UPDATE_PERIOD = 1. / 40.
FAKE_PING = 0 # milliseconds


class UserCommands(object):

    def __init__(self, uid, server_tick):
        self._uid = uid
        self._server_tick = server_tick
        self._fire = False
        self._move_vector = (0, 0) # length, angle

    def move_vector(self, length, angle):
        if not length and not angle:
            self._move_vector = (length, self._move_vector[1])
        else:
            self._move_vector = (length, angle)

    def fire(self, state):
        self._fire = state

    def activate(self):
        Clock.schedule_interval(self.send, self._server_tick / 1000.)

    def deactivate(self):
        Clock.unschedule(self.send)

    def send(self, dt):
        data = dict(cmd='user.commands',
                    args=dict(move_vector=self._move_vector,
                              fire=self._fire))
        Controller.send(**data)


class _Controller(object):

    def __init__(self):
        self._handlers = {}
        self._conn = None

    def send(self, cmd, args):
        if FAKE_PING:
            cb = lambda dt: self._conn.send(cmd, args)
            Clock.schedule_once(cb, FAKE_PING / 1000.)
        else:
            self._conn.send(cmd, args)
        log.debug('Message sent: %s, %s', cmd, args)

    def activate(self):
        Clock.schedule_interval(self.check_inbox, UPDATE_PERIOD)

    def deactivate(self):
        Clock.unschedule(self.check_inbox)

    def connect(self, address):
        self._conn = conn = FakeConnection(address)
        self._conn.connect()

    def add_handler(self, name, obj):
        self._handlers[name] = obj

    def remove_handler(self, name):
        del self._handlers[name]

    def check_inbox(self, dt):
        handlers = self._handlers
        data = self._conn.receive()
        while data is not None:
            cmd, args = data['cmd'], data['args']
            log.debug('Message received: %s, %s', cmd, args)
            handler_name, method_name = cmd.split('.')
            if handler_name in handlers:
                handler = handlers[handler_name]
                meth = getattr(handler, 'handle_' + method_name)
                if FAKE_PING:
                    cb = lambda dt: meth(**args)
                    Clock.schedule_once(cb, FAKE_PING / 1000.)
                else:
                    meth(**args)
            else:
                log.warning('No handler for command "%s"', cmd)
            data = self._conn.receive()


Controller = _Controller()
