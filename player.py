import numpy as np


class Player:
    def __init__(self, name, mpg, fg, ft, tpm, pts, treb, ast, stl, blk, to):
        self.name = name
        self.mpg = mpg
        self.fg = fg
        self.ft = ft
        self.tpm = tpm
        self.pts = pts
        self.treb = treb
        self.ast = ast
        self.stl = stl
        self.blk = blk
        self.to = to

    def get_stats(self):
        stats = np.array([[self.mpg, self.fg, self.ft, self.tpm,
                           self.pts, self.treb, self.ast, self.stl, self.blk, self.to]])
        return stats
