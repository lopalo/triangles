import json
import time

from collections import deque
from kivy.logger import Logger as log
from kivy.support import install_twisted_reactor
install_twisted_reactor()

from autobahn.twisted.websocket import (
    WebSocketClientFactory,
    WebSocketClientProtocol, connectWS)


class Connection(WebSocketClientProtocol):

    def __init__(self):
        self._inbox = deque()

    def send(self, cmd, args):
        data = dict(cmd=cmd, args=args)
        self.sendMessage(json.dumps(data).encode('utf-8'), isBinary=False)

    def receive(self):
        if not self._inbox:
            return
        return self._inbox.popleft()

    def onMessage(self, msg, isBinary=False):
        self._inbox.append(json.loads(msg.decode('utf-8')))

    @classmethod
    def connect(cls, address):
        factory = WebSocketClientFactory(address, debug=False)
        factory.protocol = cls
        connectWS(factory, timeout=5)

    def onOpen(self):
        from controller import Controller
        Controller.on_connect(self)

    def onClose(self, *args, **kwargs):
        log.info('Connection closed')


