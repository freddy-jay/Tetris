
# Tetris

This is a slightly simplified version of this game:
* No “ghost” of where the current tetromino will end up.
* No scoring.
* No difficulty curve (speeding up).
* No hold functionality.
* No box showing the next tetrominoes.
* In the official Tetris, if a tetromino is rotated but the target spot is occupied or out of bounds, the tetromino may slightly be moved to a space that is free via a “wall-
 kick” system. We do not implement this, we simply do not rotate if the target spot is occupied or out of bounds.
