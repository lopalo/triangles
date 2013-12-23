from kivy.clock import Clock
from kivy.logger import Logger as log

from network import Connection


class _Controller(object):
    UPDATE_PERIOD = 1. / 40.

    def __init__(self):
        self._handlers = {}
        self._conn = None

    def send(self, cmd, args):
        self._conn.send(cmd, args)
        log.debug('Message sent: %s, %s', cmd, args)

    def activate(self):
        Clock.schedule_interval(self.check_inbox, self.UPDATE_PERIOD)

    def deactivate(self):
        Clock.unschedule(self.check_inbox)

    def connect(self, address):
        self._conn = conn = Connection(address)
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
            handler_name, method = cmd.split('.')
            if handler_name in handlers:
                handler = handlers[handler_name]
                getattr(handler, 'handle_' + method)(**args)
            else:
                log.warning('No handler for command "%s"', cmd)
            data = self._conn.receive()


Controller = _Controller()
