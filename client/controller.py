from kivy.clock import Clock
from kivy.logger import Logger as log
from functools import wraps, partial
from network import Connection

UPDATE_PERIOD = 1. / 40.
FAKE_PING = 0 # milliseconds


class UserCommands(object):

    def __init__(self, uid, server_tick):
        self._uid = uid
        self._server_tick = server_tick
        self._fire = False
        self._move_vector = (0, 0) # length, angle

    def move_vector(self, length, angle):
        self._move_vector = (length, angle)

    def fire(self, state):
        self._fire = state

    def activate(self):
        Clock.schedule_interval(self.send, self._server_tick)

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
        self._callbacks = set() #stores weak-referenced callbacks

    def _delay(self, fun):
        callbacks = self._callbacks
        cb = lambda dt: callbacks.remove(cb) or fun()
        callbacks.add(cb)
        Clock.schedule_once(cb, FAKE_PING / 1000. / 2)

    def send(self, cmd, args):
        if FAKE_PING:
            self._delay(partial(self._conn.send, cmd, args))
        else:
            self._conn.send(cmd, args)
        log.debug('Message sent: %s, %s', cmd, args)

    def activate(self):
        Clock.schedule_interval(self.check_inbox, UPDATE_PERIOD)

    def deactivate(self):
        Clock.unschedule(self.check_inbox)

    def connect(self, address, **kwargs):
        self._conn = Connection(address)
        self._conn.connect(**kwargs)

    def add_handler(self, name, obj):
        self._handlers[name] = obj

    def remove_handler(self, name):
        del self._handlers[name]

    def check_inbox(self, dt):
        conn = self._conn
        if conn is None:
            return
        handlers = self._handlers
        data = conn.receive()
        while data is not None:
            cmd, args = data['cmd'], data['args']
            log.debug('Message received: %s, %s', cmd, args)
            parts = cmd.split('.')
            if len(parts) == 1:
                handler_name, method_name = parts[0], 'handle'
            else:
                handler_name, method_name = parts
                method_name = 'handle_' + method_name
            if handler_name in handlers:
                handler = handlers[handler_name]
                meth = safe_call(getattr(handler, method_name))
                if FAKE_PING:
                    self._delay(partial(meth, **args))
                else:
                    meth(**args)
            else:
                log.warning('No handler for command "%s"', cmd)
            data = conn.receive()


def safe_call(fun):
    @wraps(fun)
    def new_fun(*args, **kwargs):
        try:
            return fun(*args, **kwargs)
        except Exception:
            log.exception('Exception occurred in function call')
    return new_fun


Controller = _Controller()
