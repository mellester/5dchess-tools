use super::*;

/**
    Generates idle boards. Idle boards are defined as such:
    - take the present, active, playable boards owned by the current player
    - copy them and add them to the end of their timeline, effectively passing them to the opponent
**/
pub fn generate_idle_boards<'a>(
    game: &'a Game,
    partial_game: &'a PartialGame<'a>,
) -> Option<PartialGame<'a>> {
    let mut new_partial_game = PartialGame::empty(partial_game.info.clone(), None);

    for board in partial_game.own_boards(game) {
        if board.t() == partial_game.info.present {
            let mut n_board = board.clone();
            n_board.t += 1;
            n_board.en_passant = None;
            new_partial_game.insert(n_board);
            new_partial_game
                .info
                .get_timeline_mut(board.l())?
                .last_board += 1;
        }
    }

    new_partial_game.parent = Some(partial_game);
    Some(new_partial_game)
}

/**
    Checks whether or not the opponent can currently take the current player's king.
    This function can be used to verify that a position is in check by passing it the IDLE boards, generated by `generate_idle_boards`; `is_in_check` does exactly that.
    It differs from `is_illegal`, as it looks at the *opponent's* moves, instead of the *current player's* moves.
**/
pub fn is_threatened<'a>(game: &'a Game, partial_game: &'a PartialGame<'a>) -> Option<(bool, Option<Move>)> {
    for board in partial_game.opponent_boards(game) {
        for mv in board.generate_moves_flag(game, partial_game, GenMovesFlag::Check)? {
            if let Some(piece) = mv.to.0 {
                if piece.is_royal() && piece.white == partial_game.info.active_player {
                    return Some((true, Some(mv)));
                }
            }
        }
    }

    Some((false, None))
}

/**
    Checks whether or not the current player can take the opponent's king.
    This function can be used to verify that a move/moveset is legal by passing it the newly generated `PartialGame`.
    It differs from `is_threatened`, as it looks at the *current player's* moves, instead of the *opponent's* moves.
**/
pub fn is_illegal<'a>(game: &'a Game, partial_game: &'a PartialGame<'a>) -> Option<(bool, Option<Move>)> {
    for board in partial_game.own_boards(game) {
        for mv in board.generate_moves_flag(game, partial_game, GenMovesFlag::Check)? {
            if let Some(piece) = mv.to.0 {
                if piece.is_royal() && piece.white != partial_game.info.active_player {
                    return Some((true, Some(mv)));
                }
            }
        }
    }

    Some((false, None))
}

/** Returns whether or not the current player is in check. Active, playable boards on the present are passed along and a threatening move by the opponent is searched.
    - Returns `Some((true, Some(move)))` if the opponent has a checking move.
    - Returns `Some((false, None))` if the opponent hasn't a checking move.
    - Returns `None` if there is an error while generating moves
    - While there is no code that will return `Some((true, None))`, you should cover that case as well for future-proofing.
**/
pub fn is_in_check<'a>(game: &'a Game, partial_game: &'a PartialGame<'a>) -> Option<(bool, Option<Move>)> {
    if let Some(new_partial_game) = generate_idle_boards(game, partial_game) {
        is_threatened(game, &new_partial_game)
    } else {
        None
    }
}

#[macro_use]
mod macros {
    macro_rules! cast_rider_threats {
        ( $game:expr, $partial_game:expr, $board:expr, [$(($dl:expr, $dt:expr)),* $(,)?], $gonals:tt ) => {
            $(
                cast_rider_threats_sub!($game, $partial_game, $board, $dl, $dt, $gonals);
            )*
        }
    }

    macro_rules! cast_rider_threats_sub {
        ( $game:expr, $partial_game:expr, $board:expr, $dl:expr, $dt:expr, [$(($index:expr, $dx:expr, $dy:expr)),* $(,)?] ) => {
            let mut tmp = if $board.white() {
                [$(
                    ($board.bitboards.white[$index] as BitBoardPrimitive, $dx as Physical, $dy as Physical)
                ),*]
            } else {
                [$(
                    ($board.bitboards.black[$index] as BitBoardPrimitive, $dx as Physical, $dy as Physical)
                ),*]
            };

            if let Some((from_x, from_y, to)) = cast_rider_threats($game, $partial_game, $board.l(), $board.t(), $dl, $dt * 2, &mut tmp) {
                return Some((
                    true,
                    Move::new($game, $partial_game, Coords($board.l(), $board.t(), from_x, from_y), to),
                ));
            }
        }
    }

    macro_rules! cast_leaper_threats {
        ( $game:expr, $partial_game:expr, $board:expr, [$(($dl:expr, $dt:expr)),* $(,)?], $gonals:tt ) => {
            $(
                cast_leaper_threats_sub!($game, $partial_game, $board, $dl, $dt, $gonals);
            )*
        }
    }

    macro_rules! cast_leaper_threats_sub {
        ( $game:expr, $partial_game:expr, $board:expr, $dl:expr, $dt:expr, [$(($index:expr, $dx:expr, $dy:expr)),* $(,)?] ) => {
            let mut tmp = if $board.white() {
                [$(
                    ($board.bitboards.white[$index] as BitBoardPrimitive, $dx as Physical, $dy as Physical)
                ),*]
            } else {
                [$(
                    ($board.bitboards.black[$index] as BitBoardPrimitive, $dx as Physical, $dy as Physical)
                ),*]
            };

            if let Some((from_x, from_y, to)) = cast_leaper_threats($game, $partial_game, $board.l() + $dl, $board.t() + $dt * 2, &mut tmp) {
                return Some((
                    true,
                    Move::new($game, $partial_game, Coords($board.l(), $board.t(), from_x, from_y), to),
                ));
            }
        }
    }
}

/// Similar to `is_threatened`, but using bitboards instead of GenMoves
pub fn is_threatened_bitboard<'a>(game: &'a Game, partial_game: &'a PartialGame<'a>) -> Option<(bool, Option<Move>)> {
    for board in partial_game.opponent_boards(game) {
        if let Some(res) = is_threatened_bitboard_sub(game, partial_game, board) {
            return Some(res)
        }
    }

    None
}

fn is_threatened_bitboard_sub<'a>(game: &'a Game, partial_game: &'a PartialGame<'a>, board: &'a Board) -> Option<(bool, Option<Move>)> {
    if let Some((from_x, from_y, to_x, to_y)) = threats_within_board(board) {
        return Some((true, Move::new(game, partial_game, Coords(board.l(), board.t(), from_x, from_y), Coords(board.l(), board.t(), to_x, to_y))));
    }

    // Haha, you thought you were safe from number walls here?

    if board.white() {
        cast_leaper_threats!(game, partial_game, board, [
            (-1, -1),
            (-1, 1),
        ], [
            // Pawn
            (0, 0, 0),
        ]);
        cast_leaper_threats!(game, partial_game, board, [
            (-1, 0),
        ], [
            // Brawn
            (1, -1, 0),
            (1, 1, 0),
            (1, 0, 1),
        ]);
        cast_leaper_threats!(game, partial_game, board, [
            (0, -1),
        ], [
            // Brawn
            (1, 0, 1),
        ]);
    } else {
        cast_leaper_threats!(game, partial_game, board, [
            (1, -1),
            (1, 1),
        ], [
            // Pawn
            (0, 0, 0),
        ]);
        cast_leaper_threats!(game, partial_game, board, [
            (1, 0),
        ], [
            // Brawn
            (1, -1, 0),
            (1, 1, 0),
            (1, 0, -1),
        ]);
        cast_leaper_threats!(game, partial_game, board, [
            (0, -1),
        ], [
            // Brawn
            (1, 0, -1),
        ]);
    }

    cast_leaper_threats!(game, partial_game, board, [
        (0, -1),
        (1, 0),
        (-1, 0),
    ], [
        // Wazir
        (2, 0, 0),
        // Ferz
        (3, 1, 0),
        (3, -1, 0),
        (3, 0, 1),
        (3, 0, -1),
        // Rhino
        (4, 1, 1),
        (4, 1, -1),
        (4, -1, 1),
        (4, -1, -1),
        // Knight
        (10, -2, 0),
        (10, 2, 0),
        (10, 0, -2),
        (10, 0, 2),
    ]);

    cast_leaper_threats!(game, partial_game, board, [
        (1, 1),
        (1, -1),
        (-1, 1),
        (-1, -1)
    ], [
        // Ferz
        (3, 0, 0),
        // Rhino
        (4, 1, 0),
        (4, -1, 0),
        (4, 0, 1),
        (4, 0, -1),
        // Wolf
        (5, 1, 1),
        (5, 1, -1),
        (5, -1, 1),
        (5, -1, -1),
    ]);

    cast_leaper_threats!(game, partial_game, board, [
        (0, -2),
        (2, 0),
        (-2, 0),
    ], [
        // Knight
        (10, -1, 0),
        (10, 1, 0),
        (10, 0, -1),
        (10, 0, 1),
    ]);

    cast_leaper_threats!(game, partial_game, board, [
        (1, 2),
        (1, -2),
        (-1, 2),
        (-1, -2),
        (2, 1),
        (2, -1),
        (-2, 1),
        (-2, -1),
    ], [
        // Knight
        (10, 0, 0),
    ]);

    cast_rider_threats!(game, partial_game, board, [
        (0, -1),
        (1, 0),
        (-1, 0),
    ], [
        // Rook
        (6, 0, 0),
        // Bishop
        (7, 1, 0),
        (7, -1, 0),
        (7, 0, 1),
        (7, 0, -1),
        // Unicorn
        (8, 1, 1),
        (8, 1, -1),
        (8, -1, 1),
        (8, -1, -1),
    ]);

    cast_rider_threats!(game, partial_game, board, [
        (1, 1),
        (1, -1),
        (-1, 1),
        (-1, -1),
    ], [
        // Bishop
        (7, 0, 0),
        // Unicorn
        (8, 1, 0),
        (8, -1, 0),
        (8, 0, 1),
        (8, 0, -1),
        // Dragon
        (9, 1, 1),
        (9, 1, -1),
        (9, -1, 1),
        (9, -1, -1),
    ]);

    None
}

macro_rules! leaper_within_board {
    ( $board:expr, $index:expr, $royal:expr, $dx:expr, $dy:expr ) => {
        // println!("{:#066b}", $bitboard);
        let tmp = bitboard_shift(
            if $board.white() {
                $board.bitboards.white[$index]
            } else {
                $board.bitboards.black[$index]
            },
            $dx,
            $dy,
            $board.width(),
            $board.height()
        ) & $royal;
        // println!("{:#066b}", tmp);
        if tmp != 0 {
            let index = tmp.trailing_zeros();
            let ex = (index % $board.width() as u32) as i8;
            let ey = (index / $board.height() as u32) as i8;
            return Some((ex - $dx, ey - $dy, ex, ey))
        }
    }
}

/// Uses bitboards to calculate the spatial threats in a board
// *I wish our minds were better at this*
#[inline]
fn threats_within_board(board: &Board) -> Option<(Physical, Physical, Physical, Physical)> {
    let royal = if board.white() {
        board.bitboards.black_royal
    } else {
        board.bitboards.white_royal
    };

    let movable = if board.white() {
        board.bitboards.white_movable
    } else {
        board.bitboards.black_movable
    };

    if board.white() {
        leaper_within_board!(board, 0, royal, 1, 1);
        leaper_within_board!(board, 0, royal, -1, 1);
    } else {
        leaper_within_board!(board, 0, royal, 1, -1);
        leaper_within_board!(board, 0, royal, -1, -1);
    }

    // Wazir
    leaper_within_board!(board, 2, royal, 0, 1);
    leaper_within_board!(board, 2, royal, 0, -1);
    leaper_within_board!(board, 2, royal, 1, 0);
    leaper_within_board!(board, 2, royal, -1, 0);

    // Ferz
    leaper_within_board!(board, 3, royal, 1, 1);
    leaper_within_board!(board, 3, royal, 1, -1);
    leaper_within_board!(board, 3, royal, -1, 1);
    leaper_within_board!(board, 3, royal, -1, -1);

    // Knight
    leaper_within_board!(board, 10, royal, 2, 1);
    leaper_within_board!(board, 10, royal, 2, -1);
    leaper_within_board!(board, 10, royal, -2, 1);
    leaper_within_board!(board, 10, royal, -2, -1);
    leaper_within_board!(board, 10, royal, 1, 2);
    leaper_within_board!(board, 10, royal, 1, -2);
    leaper_within_board!(board, 10, royal, -1, 2);
    leaper_within_board!(board, 10, royal, -1, -2);

    // Rider pieces
    const N_RIDERS: usize = 8;
    const RIDERS: [(usize, Physical, Physical); N_RIDERS] = [
        (6, -1, 0),
        (6, 1, 0),
        (6, 0, 1),
        (6, 0, -1),
        (7, -1, 1),
        (7, -1, -1),
        (7, 1, 1),
        (7, 1, -1),
    ];

    let mut bitboards: [BitBoardPrimitive; N_RIDERS] = [0; N_RIDERS];
    let mut attacks: [BitBoardPrimitive; N_RIDERS] = [0; N_RIDERS];

    if board.white() {
        for n in 0..N_RIDERS {
            bitboards[n] = board.bitboards.white[RIDERS[n].0];
        }
    } else {
        for n in 0..N_RIDERS {
            bitboards[n] = board.bitboards.black[RIDERS[n].0];
        }
    }

    for n in 1..=(MAX_BITBOARD_WIDTH as Physical) {
        for o in 0..N_RIDERS {
            // println!("==> {:#066b} / ({}:{}) / ({}:{})", bitboards[o], RIDERS[o].1, RIDERS[o].2, board.l(), board.t());
            bitboards[o] = bitboard_shift(bitboards[o], RIDERS[o].1, RIDERS[o].2, board.width(), board.height()) & movable;
            attacks[o] = bitboards[o] & royal;
            // println!("... {:#066b}", bitboards[o]);
            // println!("... {:#066b}", movable);
            // println!("... {:#066b}", royal);
            // println!("... {:#066b}", attacks[o]);
        }

        let mut zero: bool = true;
        for o in 0..N_RIDERS {
            if attacks[o] != 0 {
                let index = attacks[o].trailing_zeros();
                let ex = (index % board.width() as u32) as Physical;
                let ey = (index / board.height() as u32) as Physical;
                return Some((ex - RIDERS[o].1 * n, ey - RIDERS[o].2 * n, ex, ey))
            }
            if bitboards[o] != 0 {
                zero = false;
            }
        }

        if zero {
            break
        }
    }

    None
}

#[inline]
fn cast_rider_threats<'a>(
    game: &'a Game,
    partial_game: &'a PartialGame<'a>,
    mut l: Layer,
    mut t: Time,
    dl: Layer,
    dt: Time,
    bitboards: &mut [(BitBoardPrimitive, Physical, Physical)]
) -> Option<(Physical, Physical, Coords)> {
    let mut n: usize = 0;
    loop {
        n += 1;
        l += dl;
        t += dt;

        match partial_game.get_board_with_game(game, (l, t)) {
            Some(board) => {
                let movable = if board.white() {
                    board.bitboards.white_movable
                } else {
                    board.bitboards.black_movable
                };

                let royal = if board.white() {
                    board.bitboards.black_royal
                } else {
                    board.bitboards.white_royal
                };

                let mut zero = true;
                for (bb, dx, dy) in &mut *bitboards {
                    // println!("==> {:#066b} / ({}:{}) / ({}:{})", *bb, *dx, *dy, l, t);
                    *bb = bitboard_shift(*bb, *dx, *dy, board.width(), board.height()) & movable;
                    // println!("... {:#066b}", *bb);
                    let attack = *bb & royal;
                    // println!("... {:#066b}", movable);
                    // println!("... {:#066b}", royal);
                    // println!("... {:#066b}", attack);
                    // println!("");
                    if attack != 0 {
                        let index = attack.trailing_zeros();
                        let ex = (index % board.width() as u32) as Physical;
                        let ey = (index / board.width() as u32) as Physical;
                        let sx = ex - *dx * n as Physical;
                        let sy = ey - *dy * n as Physical;
                        return Some((sx, sy, Coords(l, t, ex, ey)))
                    }
                    if *bb != 0 {
                        zero = false;
                    }
                }

                if zero {
                    break None
                }
            },
            None => break None,
        }
    }
}


#[inline]
fn cast_leaper_threats<'a>(
    game: &'a Game,
    partial_game: &'a PartialGame<'a>,
    l: Layer,
    t: Time,
    bitboards: &mut [(BitBoardPrimitive, Physical, Physical)]
) -> Option<(Physical, Physical, Coords)> {

    match partial_game.get_board_with_game(game, (l, t)) {
        Some(board) => {
            let movable = if board.white() {
                board.bitboards.white_movable
            } else {
                board.bitboards.black_movable
            };

            let royal = if board.white() {
                board.bitboards.black_royal
            } else {
                board.bitboards.white_royal
            };

            for (bb, dx, dy) in &mut *bitboards {
                // println!("==> {:#066b} / ({}:{}) / ({}:{})", *bb, *dx, *dy, l, t);
                *bb = bitboard_shift(*bb, *dx, *dy, board.width(), board.height()) & movable;
                // println!("... {:#066b}", *bb);
                let attack = *bb & royal;
                // println!("... {:#066b}", movable);
                // println!("... {:#066b}", royal);
                // println!("... {:#066b}", attack);
                // println!("");
                if attack != 0 {
                    let index = attack.trailing_zeros();
                    let ex = (index % board.width() as u32) as Physical;
                    let ey = (index / board.width() as u32) as Physical;
                    let sx = ex - *dx as Physical;
                    let sy = ey - *dy as Physical;
                    return Some((sx, sy, Coords(l, t, ex, ey)))
                }
            }
            None
        },
        None => None,
    }
}
