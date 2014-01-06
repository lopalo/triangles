import json
import socket

from threading import Thread
from collections import deque
from kivy.vector import Vector
from websocket import WebSocketApp
from kivy.logger import Logger as log


class Connection(object):

    def __init__(self, address):
        self._address = address
        self._inbox = deque()
        self._outbox = deque()
        self._connected = False
        self._web_sock = None

    def send(self, cmd, args):
        self._outbox.append(dict(cmd=cmd, args=args))

    def receive(self):
        if not self._inbox:
            return
        return self._inbox.popleft()

    def _sender(self):
        outbox = self._outbox
        while self._connected:
            if not outbox:
                continue
            output = json.dumps(outbox.popleft())
            self._web_sock.send(output.encode('utf-8'))

    def _receiver(self):
        def on_message(ws, msg):
            self._inbox.append(json.loads(msg.decode('utf-8')))
        def on_open(ws):
            Thread(target=self._sender, args=()).start()
        self._web_sock = WebSocketApp(self._address,
                                      on_message=on_message,
                                      on_open=on_open, on_error=self.disconnect,
                                      on_close=self.disconnect)
        self._web_sock.run_forever(sockopt=(socket.TCP_NODELAY,))

    def connect(self):
        Thread(target=self._receiver, args=()).start()

    def disconnect(self, reason=None):
        self._connected = False
        if reason is not None:
            log.info('Connection closed, reason: %s', reason)
        else:
            log.info('Connection closed')
        #TODO: popup


class FakeConnection(object):

    def __init__(self, address):
        self._address = address
        self._inbox = deque()
        self._bbox = BlackBox(self._inbox)

    def send(self, cmd, args):
        self._bbox.handle(cmd, args)

    def receive(self):
        if not self._inbox:
            return
        return self._inbox.popleft()

    def connect(self):
        pass

    def disconnect(self):
        pass


class BlackBox(object):
    #TODO: change this mock to connection with server
    SPEED_FACTOR = 0.1
    SERVER_TICK = 200 # milliseconds

    def __init__(self, inbox):
        self._inbox = inbox
        self._user_pos = Vector(100, 100)
        self._user_angle = 0

    def handle(self, cmd, args):
        inbox = self._inbox
        if cmd == "world.start":
            name = args['name']
            args = dict(uid="user:123456",
                        server_tick=self.SERVER_TICK,
                        level_size=(1000, 700))
            inbox.append(dict(cmd='world.init', args=args))
            #TODO: send info for player's object
        elif cmd == 'user.commands':
            length, angle = args['move_vector']
            speed = Vector(1, 0).rotate(angle) * length * self.SPEED_FACTOR
            self._user_pos += speed * self.SERVER_TICK
            self._user_angle = angle
            data = {"user:123456": {"pos": self._user_pos,
                                    "angle": self._user_angle},
                    "user:55": {"pos": self._user_pos - Vector(100, 100),
                                "angle": self._user_angle - 70}}
            msg = dict(cmd='world.tick', args=dict(tick_data=data))
            inbox.append(msg)
        elif cmd == 'world.get_objects_info':
            idents = args['idents']
            data = {"user:55": {"type": "triangle",
                                "pos": self._user_pos - Vector(100, 100),
                                "angle": self._user_angle - 70}}
            msg = dict(cmd='world.objects_info', args=dict(objects=data))
            inbox.append(msg)

