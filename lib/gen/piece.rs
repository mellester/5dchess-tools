use super::*;

/** A combination of a piece and its coordinates, used to generate a piece's moves.
    Unless you are implementing a new fairy piece, you should call `PiecePosition::generate_moves` to generate a piece's moves
    This structure implements the `GenMoves` trait, and thus lets you generate moves using a `PiecePosition` instance,
    a `Game` state and a `PartialGame` state.

    ## Example

    ```
    let position = Coords(0, 0, 1, 0);
    let piece = PiecePosition::new(game.get(position).piece().unwrap(), position);

    // This loop will now print all of the moves that the `c1`-knight can make
    for mv in piece.generate_moves(game, &no_partial_game(game)).unwrap() {
        println!("{:?}", mv);
    }
    ```
**/
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PiecePosition(pub Piece, pub Coords);

impl PiecePosition {
    pub fn new(piece: Piece, coords: Coords) -> PiecePosition {
        PiecePosition(piece, coords)
    }
}

/** An iterator that yields the movements of pawn-like pieces (ie. pawns and brawns), including en-passant.
    It is created by `PiecePosition::generate_moves`.
**/
#[derive(Clone)]
pub struct PawnIter {
    moves: Vec<Move>,
    state: usize,
}

/// Brawn's capture movements
const BRAWN_CAPTURES: [Coords; 8] = [
    Coords(0, 0, 1, 1),
    Coords(0, 0, -1, 1),
    Coords(0, -2, 0, 1),
    Coords(-1, 0, 0, 1),
    Coords(-1, 2, 0, 0),
    Coords(-1, -2, 0, 0),
    Coords(-1, 0, 1, 0),
    Coords(-1, 0, -1, 0),
];

/// Pawn's capture movements
const PAWN_CAPTURES: [Coords; 4] = [
    Coords(0, 0, 1, 1),
    Coords(0, 0, -1, 1),
    Coords(-1, 2, 0, 0),
    Coords(-1, -2, 0, 0),
];

#[inline]
fn forward(a: Coords, b: Coords, color: bool) -> Coords {
    if color {
        a + b
    } else {
        a - b
    }
}

impl PawnIter {
    /** Creates a new PawnIter; unless you are implementing a new fairy piece, you should call `PiecePosition::generate_moves` to generate a piece's moves **/
    pub fn new<'a>(
        piece: PiecePosition,
        game: &'a Game,
        partial_game: &'a PartialGame<'a>,
        flag: GenMovesFlag,
    ) -> Option<Self> {
        let mut moves: Vec<Move> = Vec::new();

        // Forward moves
        if flag != GenMovesFlag::Check {
            for perm in vec![Coords(0, 0, 0, 1), Coords(-1, 0, 0, 0)] {
                let dest = forward(piece.1, perm, piece.0.white);
                if partial_game.get_with_game(game, dest).is_blank() {
                    moves.push(Move::new(game, partial_game, piece.1, dest)?);

                    let dest2 = forward(piece.1, perm + perm, piece.0.white);

                    // Kickstart move
                    if !piece.0.moved && partial_game.get_with_game(game, dest2).is_blank() {
                        moves.push(Move::new(game, partial_game, piece.1, dest2)?);
                    }
                }
            }
        }

        // Captures
        for perm in if piece.0.kind == PieceKind::Brawn {
            &BRAWN_CAPTURES[0..]
        } else {
            &PAWN_CAPTURES[0..]
        } {
            let dest = forward(piece.1, *perm, piece.0.white);
            if partial_game
                .get_with_game(game, dest)
                .is_piece_of_color(!piece.0.white)
            {
                moves.push(Move::new(game, partial_game, piece.1, dest)?);
            }
        }

        // En-passant
        if flag != GenMovesFlag::Check {
            if piece.0.can_enpassant() {
                if let Some((x, y)) = partial_game
                    .get_board_with_game(game, piece.1.non_physical())?
                    .en_passant()
                {
                    if (x, y) == forward(piece.1, Coords(0, 0, 1, 1), piece.0.white).physical()
                        || (x, y) == forward(piece.1, Coords(0, 0, -1, 1), piece.0.white).physical()
                    {
                        moves.push(Move::new(
                            game,
                            partial_game,
                            piece.1,
                            Coords(piece.1.l(), piece.1.t(), x, y),
                        )?);
                    }
                }
            }
        }

        Some(PawnIter { moves, state: 0 })
    }
}

impl Iterator for PawnIter {
    type Item = Move;

    #[inline]
    fn next(&mut self) -> Option<Move> {
        if self.state >= self.moves.len() {
            None
        } else {
            self.state += 1;
            Some(self.moves[self.state - 1])
        }
    }
}

/**
    An iterator that yields the move of a ranging piece (ie. Rooksishops, etc.).
    This iterator is created by `PiecePosition::generate_moves`.
**/
#[derive(Clone)]
pub struct RangingPieceIter<'a> {
    piece: Piece,
    coords: Coords,
    game: &'a Game,
    partial_game: &'a PartialGame<'a>,
    cardinalities: Vec<(isize, isize, isize, isize)>,
    cardinalities_index: usize,
    distance: usize,
    flag: GenMovesFlag,
}

impl<'a> RangingPieceIter<'a> {
    /** Creates a new RangingPieceIter; unless you are implementing a new fairy piece, you should call `PiecePosition::generate_moves` to generate a piece's moves **/
    #[inline]
    pub fn new(
        piece: PiecePosition,
        game: &'a Game,
        partial_game: &'a PartialGame<'a>,
        cardinalities: Vec<(isize, isize, isize, isize)>,
        flag: GenMovesFlag,
    ) -> Self {
        RangingPieceIter {
            piece: piece.0,
            coords: piece.1,
            game,
            partial_game,
            cardinalities,
            cardinalities_index: 0,
            distance: 0,
            flag,
        }
    }

    fn try_next(&mut self) -> Option<Move> {
        let cardinality = self.cardinalities[self.cardinalities_index];
        let mut next_cardinality = false;

        let res = if self.flag == GenMovesFlag::Check {
            loop {
                // IMPORTANT: remember to set next_cardinality to true if there is no next move!
                self.distance += 1;
                let n_coords = self.coords + Coords::from(cardinality) * (self.distance as isize);

                if let Some(board) = self.partial_game.get_board_with_game(self.game, n_coords.non_physical()) {
                    if cfg!(castling) {
                        match board.castle {
                            Some((x1, y1, x2, y2)) => {
                                if
                                    n_coords.2 == x1 && n_coords.3 == y1
                                    || n_coords.3 == x2 && n_coords.3 == y2
                                {
                                    break Move::new(self.game, self.partial_game, self.coords, n_coords);
                                }
                            }
                            None => {},
                        }
                    }

                    match board.get(n_coords.physical()) {
                        Tile::Void => {
                            next_cardinality = true;
                            break None;
                        }
                        Tile::Blank => {}
                        Tile::Piece(p) => {
                            next_cardinality = true;
                            if p.white != self.piece.white {
                                // Replace the following by Move::new if a ranging piece can promote
                                // break Move::new(self.game, self.partial_game, self.coords, n_coords)
                                break Some(Move::from_raw((self.piece, self.coords), (Some(p), n_coords), MoveKind::Normal))
                            } else {
                                break None
                            }
                        }
                    }
                } else {
                    next_cardinality = true;
                    break None
                }
            }
        } else {
            self.distance += 1;
            let n_coords = self.coords + Coords::from(cardinality) * (self.distance as isize);

            match self.partial_game.get_with_game(self.game, n_coords) {
                Tile::Void => {
                    next_cardinality = true;
                    None
                }
                Tile::Blank => {
                    // Replace the following by Move::new if a ranging piece can promote
                    // Move::new(self.game, self.partial_game, self.coords, n_coords)
                    Some(Move::from_raw((self.piece, self.coords), (None, n_coords), MoveKind::Normal))
                }
                Tile::Piece(p) => {
                    next_cardinality = true;
                    if p.white != self.piece.white {
                        // Replace the following by Move::new if a ranging piece can promote
                        // Move::new(self.game, self.partial_game, self.coords, n_coords)
                        Some(Move::from_raw((self.piece, self.coords), (Some(p), n_coords), MoveKind::Normal))
                    } else {
                        None
                    }
                }
            }
        };

        if next_cardinality {
            self.distance = 0;
            self.cardinalities_index += 1;
        }

        res
    }
}

impl<'a> Iterator for RangingPieceIter<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Move> {
        if self.cardinalities.len() <= self.cardinalities_index {
            return None;
        }

        loop {
            match self.try_next() {
                Some(res) => return Some(res),
                None => {
                    if self.cardinalities.len() <= self.cardinalities_index {
                        return None;
                    }
                }
            }
        }
    }
}

/** Iterator that yields the moves of a leaper piece (eg. knights, royal kings).
    This iterator is created by `PiecePosition::generate_moves`.
**/
#[derive(Clone)]
pub struct OneStepPieceIter<'a> {
    piece: Piece,
    coords: Coords,
    game: &'a Game,
    partial_game: &'a PartialGame<'a>,
    cardinalities: Vec<(isize, isize, isize, isize)>,
    cardinalities_index: usize,
}

impl<'a> OneStepPieceIter<'a> {
    /** Creates a new OneStepPieceIter; unless you are implementing a new fairy piece, you should call `PiecePosition::generate_moves` to generate a piece's moves **/
    #[inline]
    pub fn new(
        piece: PiecePosition,
        game: &'a Game,
        partial_game: &'a PartialGame<'a>,
        cardinalities: Vec<(isize, isize, isize, isize)>,
    ) -> Self {
        OneStepPieceIter {
            piece: piece.0,
            coords: piece.1,
            game,
            partial_game,
            cardinalities,
            cardinalities_index: 0,
        }
    }

    fn try_next(&mut self) -> Option<Move> {
        self.cardinalities_index += 1;

        let cardinality = self.cardinalities[self.cardinalities_index - 1];

        let n_coords = self.coords + Coords::from(cardinality);

        let res = match self.partial_game.get_with_game(self.game, n_coords) {
            Tile::Void => None,
            Tile::Blank => Some(Move::with_piece(self.game, (self.piece, self.coords), (None, n_coords))),
            Tile::Piece(p) => {
                if p.white != self.piece.white {
                    Some(Move::with_piece(self.game, (self.piece, self.coords), (Some(p), n_coords)))
                } else {
                    None
                }
            }
        };

        res
    }
}

impl<'a> Iterator for OneStepPieceIter<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Move> {
        if self.cardinalities.len() <= self.cardinalities_index {
            return None;
        }

        loop {
            match self.try_next() {
                Some(res) => return Some(res),
                None => {
                    if self.cardinalities.len() <= self.cardinalities_index {
                        return None;
                    }
                }
            }
        }
    }
}

/** Iterator yielding the special movements of a king (short castle and long castle). **/
#[derive(Clone)]
pub struct KingIter<'a> {
    pub castling_direction: u8,
    pub piece: Piece,
    pub game: &'a Game,
    pub partial_game: &'a PartialGame<'a>,
    pub coords: Coords,
    pub flag: GenMovesFlag,
}

impl<'a> KingIter<'a> {
    #[inline]
    pub fn new(
        piece: PiecePosition,
        game: &'a Game,
        partial_game: &'a PartialGame<'a>,
        flag: GenMovesFlag,
    ) -> Self {
        Self {
            castling_direction: 0,
            piece: piece.0,
            coords: piece.1,
            game,
            partial_game,
            flag,
        }
    }
}

#[cfg(castling)]
impl<'a> Iterator for KingIter<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Move> {
        if self.castling_direction > 2 || self.flag == GenMovesFlag::Check {
            return None;
        }

        self.castling_direction += 1;

        if self.castling_direction == 1 {
            // castle left
            let (mut x, y) = self.coords.physical();
            let board = self
                .partial_game
                .get_board_with_game(self.game, self.coords.non_physical())?;
            if x > 1 && board.get((x - 1, y)).is_empty() && board.get((x - 2, y)).is_empty() {
                x -= 2;
                while x >= 0 && board.get((x, y)).is_blank() {
                    x -= 1;
                }
                if board
                    .get((x, y))
                    .piece()
                    .map(|p| p.can_castle_to() && !p.moved && p.white == self.piece.white)
                    .unwrap_or(false)
                {
                    return Move::new(
                        self.game,
                        self.partial_game,
                        self.coords,
                        Coords(self.coords.l(), self.coords.t(), self.coords.x() - 2, y),
                    );
                }
            }
        } else if self.castling_direction == 2 {
            // castle right
            let (mut x, y) = self.coords.physical();
            let board = self
                .partial_game
                .get_board_with_game(self.game, self.coords.non_physical())?;
            if x > 1 && board.get((x + 1, y)).is_empty() && board.get((x + 2, y)).is_empty() {
                x += 2;
                while x < board.width() && board.get((x, y)).is_blank() {
                    x += 1;
                }
                if board
                    .get((x, y))
                    .piece()
                    .map(|p| p.can_castle_to() && !p.moved && p.white == self.piece.white)
                    .unwrap_or(false)
                {
                    return Move::new(
                        self.game,
                        self.partial_game,
                        self.coords,
                        Coords(self.coords.l(), self.coords.t(), self.coords.x() + 2, y),
                    );
                }
            }
        }

        // TCR
        self.next()
    }
}

#[cfg(not(castling))]
impl<'a> Iterator for KingIter<'a> {
    type Item = Move;

    #[inline]
    fn next(&mut self) -> Option<Move> {
        None
    }
}

/**
    Iterator combining the different move kinds of all of the pieces.
    If your custom piece doesn't follow a usual move pattern, then you'll have to add your own value in this enum.
**/
#[derive(Clone)]
pub enum PieceMoveIter<'a> {
    Pawn(PawnIter),
    Chain(std::iter::Chain<Box<PieceMoveIter<'a>>, Box<PieceMoveIter<'a>>>),
    King(KingIter<'a>),
    Ranging(RangingPieceIter<'a>),
    OneStep(OneStepPieceIter<'a>),
}

impl<'a> Iterator for PieceMoveIter<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Move> {
        match self {
            PieceMoveIter::Pawn(i) => i.next(),
            PieceMoveIter::Ranging(i) => i.next(),
            PieceMoveIter::OneStep(i) => i.next(),
            PieceMoveIter::King(i) => i.next(),
            PieceMoveIter::Chain(i) => i.next(),
        }
    }
}

impl<'a> GenMoves<'a> for PiecePosition {
    type Iter = PieceMoveIter<'a>;

    /**
        Generates the moves for a single piece, given a partial game state and its complementary game state.
        You should be using this function if you wish to generate the moves of a piece.

        ## Example

        ```
        let game: &Game;
        let partial_game: &PartialGame;
        // Fill in game and partial_game here

        let position = Coords(0, 0, 3, 1); // (0T1w)d1
        let piece = partial_game.get_with_game(game, position);
        let piece_position = PiecePosition::new(piece.piece().unwrap(), position);

        for mv in piece_position.generate_moves(game, partial_game) {
            println!("{}", mv);
        }
        ```
    **/
    fn generate_moves_flag(
        self,
        game: &'a Game,
        partial_game: &'a PartialGame<'a>,
        flag: GenMovesFlag,
    ) -> Option<PieceMoveIter<'a>> {
        Some(match self.0.kind {
            PieceKind::Pawn | PieceKind::Brawn => {
                PieceMoveIter::Pawn(PawnIter::new(self, game, partial_game, flag)?)
            }
            PieceKind::Knight => PieceMoveIter::OneStep(OneStepPieceIter::new(
                self,
                game,
                partial_game,
                PERMUTATIONS[0].clone(),
            )),
            PieceKind::Rook => PieceMoveIter::Ranging(RangingPieceIter::new(
                self,
                game,
                partial_game,
                PERMUTATIONS[1].clone(),
                flag,
            )),
            PieceKind::Bishop => PieceMoveIter::Ranging(RangingPieceIter::new(
                self,
                game,
                partial_game,
                PERMUTATIONS[2].clone(),
                flag,
            )),
            PieceKind::Unicorn => PieceMoveIter::Ranging(RangingPieceIter::new(
                self,
                game,
                partial_game,
                PERMUTATIONS[3].clone(),
                flag,
            )),
            PieceKind::Dragon => PieceMoveIter::Ranging(RangingPieceIter::new(
                self,
                game,
                partial_game,
                PERMUTATIONS[4].clone(),
                flag,
            )),
            PieceKind::Princess => PieceMoveIter::Ranging(RangingPieceIter::new(
                self,
                game,
                partial_game,
                PERMUTATIONS[1]
                    .iter()
                    .chain(PERMUTATIONS[2].iter())
                    .cloned()
                    .collect(),
                flag,
            )),
            PieceKind::Queen | PieceKind::RoyalQueen => {
                PieceMoveIter::Ranging(RangingPieceIter::new(
                    self,
                    game,
                    partial_game,
                    PERMUTATIONS[1]
                        .iter()
                        .chain(PERMUTATIONS[2].iter())
                        .chain(PERMUTATIONS[3].iter())
                        .chain(PERMUTATIONS[4].iter())
                        .cloned()
                        .collect(),
                    flag,
                ))
            }
            PieceKind::King => {
                if cfg!(castling) {
                    PieceMoveIter::Chain(
                        Box::new(PieceMoveIter::King(KingIter::new(
                            self,
                            game,
                            partial_game,
                            flag,
                        )))
                        .chain(Box::new(PieceMoveIter::OneStep(OneStepPieceIter::new(
                            self,
                            game,
                            partial_game,
                            PERMUTATIONS[1]
                                .iter()
                                .chain(PERMUTATIONS[2].iter())
                                .chain(PERMUTATIONS[3].iter())
                                .chain(PERMUTATIONS[4].iter())
                                .cloned()
                                .collect(),
                        )))))
                } else {
                    PieceMoveIter::OneStep(OneStepPieceIter::new(
                        self,
                        game,
                        partial_game,
                        PERMUTATIONS[1]
                            .iter()
                            .chain(PERMUTATIONS[2].iter())
                            .chain(PERMUTATIONS[3].iter())
                            .chain(PERMUTATIONS[4].iter())
                            .cloned()
                            .collect(),
                    ))
                }
            }
            PieceKind::CommonKing => PieceMoveIter::OneStep(OneStepPieceIter::new(
                self,
                game,
                partial_game,
                PERMUTATIONS[1]
                    .iter()
                    .chain(PERMUTATIONS[2].iter())
                    .chain(PERMUTATIONS[3].iter())
                    .chain(PERMUTATIONS[4].iter())
                    .cloned()
                    .collect(),
            )),
        })
    }

    // TODO: a more optimized way of determining whether or not the move is legal
    // I don't want to do this right now
}

lazy_static! {
    /// Permutations for the symmetric pieces of the base game
    pub static ref PERMUTATIONS: Vec<Vec<(isize, isize, isize, isize)>> = {
        [
            (
                vec![
                    (2, 1, 0, 0),
                    (1, 2, 0, 0),
                    (0, 2, 1, 0),
                    (0, 1, 2, 0),
                    (0, 0, 2, 1),
                    (0, 0, 1, 2),
                    (1, 0, 0, 2),
                    (2, 0, 0, 1),
                    (2, 0, 1, 0),
                    (1, 0, 2, 0),
                    (0, 2, 0, 1),
                    (0, 1, 0, 2),
                ],
                2,
            ),
            (
                vec![(1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1)],
                1,
            ),
            (
                vec![
                    (1, 1, 0, 0),
                    (0, 1, 1, 0),
                    (0, 0, 1, 1),
                    (1, 0, 0, 1),
                    (1, 0, 1, 0),
                    (0, 1, 0, 1),
                ],
                2,
            ),
            (
                vec![(0, 1, 1, 1), (1, 0, 1, 1), (1, 1, 0, 1), (1, 1, 1, 0)],
                3,
            ),
            (vec![(1, 1, 1, 1)], 4),
        ]
        .iter()
        .map(|(group, cardinality)| {
            let mut res: Vec<(isize, isize, isize, isize)> =
                Vec::with_capacity(group.len() * 2usize.pow(*cardinality));
            for element in group {
                for perm_index in 0..(2usize.pow(*cardinality)) {
                    let mut perm: Vec<isize> = Vec::with_capacity(4);
                    let mut o = 0usize;
                    for i in vec![element.0, element.1 * 2, element.2, element.3] {
                        if i != 0 {
                            perm.push(if (perm_index >> o) % 2 == 1 { -i } else { i });
                            o += 1;
                        } else {
                            perm.push(0);
                        }
                    }
                    res.push((perm[0], perm[1], perm[2], perm[3]));
                }
            }
            res
        })
        .collect()
    };
}
