import json
import socket
import time

from threading import Thread
from collections import deque
from websocket import WebSocketApp, setdefaulttimeout
from kivy.logger import Logger as log


class ConnectionTimeout(Exception):
    pass


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
            self._connected = True
            send_thread = Thread(target=self._sender, args=())
            send_thread.setDaemon(True)
            send_thread.start()
        self._web_sock = WebSocketApp(self._address,
                                      on_message=on_message,
                                      on_open=on_open,
                                      on_error=self.disconnect,
                                      on_close=self.disconnect)
        sockopt = ((socket.IPPROTO_TCP, socket.TCP_NODELAY, 1),)
        self._web_sock.run_forever(sockopt=sockopt)

    def connect(self, wait=False, timeout=2000):
        timeout = float(timeout) / 1000.
        setdefaulttimeout(timeout)
        receive_thread = Thread(target=self._receiver, args=())
        receive_thread.setDaemon(True)
        receive_thread.start()
        if not wait:
            return
        deadline = time.time() + timeout
        while time.time() < deadline:
            if self._connected:
                return
        raise ConnectionTimeout

    def disconnect(self, web_sock):
        self._connected = False
        log.info('Connection closed')

