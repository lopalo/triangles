from collections import deque
from kivy.vector import Vector
from kivy.clock import Clock


class UserCommands(object):

    def __init__(self, uid, server_tick, send_fun):
        self._uid = uid
        self._server_tick = server_tick
        self._send_fun = send_fun
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
        data = dict(cmd='user_commands',
                    args=dict(move_vector=self._move_vector,
                              fire=self._fire))
        self._send_fun(**data)


class Connection(object):

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
    SERVER_TICK = 200

    def __init__(self, inbox):
        self._inbox = inbox
        self._user_pos = Vector(100, 100)

    def handle(self, cmd, args):
        inbox = self._inbox
        if cmd == "start_game":
            name = args['name']
            args = dict(uid="user:123456",
                        server_tick=self.SERVER_TICK,
                        level_size=(1000, 700))
            inbox.append(dict(cmd='init', args=args))
        elif cmd == 'user_commands':
            length, angle = args['move_vector']
            speed = Vector(1, 0).rotate(angle) * length * self.SPEED_FACTOR
            self._user_pos += speed * self.SERVER_TICK
            data = {"user:123456": {"pos": self._user_pos, "angle": angle},
                    "user:55": {"pos": self._user_pos - Vector(100, 100),
                                "angle": angle - 70}}
            msg = dict(cmd='tick', args=dict(tick_data=data))
            inbox.append(msg)


