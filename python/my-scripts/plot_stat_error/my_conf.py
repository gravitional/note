class Count():

    def __init__(self) -> None:
        self._cnt: int = 0

    def __call__(self):
        val = self._cnt
        self._cnt += 1
        return val
