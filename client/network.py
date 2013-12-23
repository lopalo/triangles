from collections import deque
from kivy.vector import Vector
from kivy.logger import Logger as log


class Connection(object):

    def __init__(self, address):
        self._address = address
        self._inbox = deque()
        self._bbox = BlackBox(self._inbox)
        #TODO: use 2 threads: for sending and receiving (both are blocking)

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
    SERVER_TICK = 200

    def __init__(self, inbox):
        self._inbox = inbox
        self._user_pos = Vector(100, 100)

    def handle(self, cmd, args):
        inbox = self._inbox
        if cmd == "world.start":
            name = args['name']
            args = dict(uid="user:123456",
                        server_tick=self.SERVER_TICK,
                        level_size=(1000, 700))
            inbox.append(dict(cmd='world.init', args=args))
        elif cmd == 'world.user_commands':
            length, angle = args['move_vector']
            speed = Vector(1, 0).rotate(angle) * length * self.SPEED_FACTOR
            self._user_pos += speed * self.SERVER_TICK
            data = {"user:123456": {"pos": self._user_pos, "angle": angle},
                    "user:55": {"pos": self._user_pos - Vector(100, 100),
                                "angle": angle - 70}}
            msg = dict(cmd='world.tick', args=dict(tick_data=data))
            inbox.append(msg)
        elif cmd == 'world.get_objects_info':
            idents = args['idents']
            data = {"user:55": {}}
            msg = dict(cmd='world.objects_info', args=dict(objects=data))
            inbox.append(msg)

