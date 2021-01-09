use super::{Board, Layer, Physical, Time};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Coords(pub Layer, pub Time, pub Physical, pub Physical); // ⟨l, t, x, y⟩

impl Coords {
    pub fn new(l: Layer, t: Time, x: Physical, y: Physical) -> Self {
        Self(l, t, x, y)
    }

    #[inline]
    pub fn physical(self) -> (Physical, Physical) {
        (self.2, self.3)
    }

    #[inline]
    pub fn non_physical(self) -> (Layer, Time) {
        (self.0, self.1)
    }

    #[inline]
    pub fn l(self) -> Layer {
        self.0
    }

    #[inline]
    pub fn t(self) -> Time {
        self.1
    }

    #[inline]
    pub fn x(self) -> Physical {
        self.2
    }

    #[inline]
    pub fn y(self) -> Physical {
        self.3
    }
}

impl From<((Layer, Time), (Physical, Physical))> for Coords {
    fn from(((l, t), (x, y)): ((Layer, Time), (Physical, Physical))) -> Self {
        Self(l, t, x, y)
    }
}

impl From<(Layer, Time, Physical, Physical)> for Coords {
    fn from((l, t, x, y): (Layer, Time, Physical, Physical)) -> Self {
        Self(l, t, x, y)
    }
}

impl From<(isize, isize, isize, isize)> for Coords {
    fn from((l, t, x, y): (isize, isize, isize, isize)) -> Self {
        Self(l as Layer, t as Time, x as Physical, y as Physical)
    }
}

impl From<(Board, (Physical, Physical))> for Coords {
    fn from((board, (x, y)): (Board, (Physical, Physical))) -> Self {
        Self(board.l, board.t, x, y)
    }
}

impl std::ops::Add<Coords> for Coords {
    type Output = Coords;

    fn add(self, w: Coords) -> Coords {
        Self(self.0 + w.0, self.1 + w.1, self.2 + w.2, self.3 + w.3)
    }
}

impl std::ops::Sub<Coords> for Coords {
    type Output = Coords;

    fn sub(self, w: Coords) -> Coords {
        Self(self.0 - w.0, self.1 - w.1, self.2 - w.2, self.3 - w.3)
    }
}

impl std::ops::Mul<isize> for Coords {
    type Output = Coords;

    fn mul(self, w: isize) -> Coords {
        Self(
            self.0 * w as Layer,
            self.1 * w as Time,
            self.2 * w as Physical,
            self.3 * w as Physical,
        )
    }
}
