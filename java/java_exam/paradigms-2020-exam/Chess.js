"use strict";
const distance = (a, b) => (Math.abs(b.charCodeAt(0) - a.charCodeAt(0)));

function Piece(color, sign, vert, hor) {
    this.color = color;
    this.sign = sign;
    this.vert = vert;
    this.hor = hor;
}

Piece.prototype.move = function (vert, hor) {
    this.check_move(vert, hor);
    this.hor = hor;
    this.vert = vert;
}
Piece.prototype.toString = function () {
    return this.color + this.sign + this.vert + this.hor;
}

function Rook(color, vert, hor) {
    Piece.call(this, color, "R", vert, hor);
}

Rook.prototype = Object.create(Piece.prototype);
Rook.prototype.check_move = function (vert, hor) {
    return ((vert === this.vert || hor === this.hor) && (vert !== this.vert || hor !== this.hor))
}
Rook.prototype.toString = function () {
    return "(" + this.sign + " " + this.vert + this.hor + ")";
}

function King(color, vert, hor) {
    Piece.call(this, color, "K", vert, hor);
}

King.prototype = Object.create(Piece.prototype);
King.prototype.check_move = function (vert, hor) {
    return (distance(vert, this.vert) + Math.abs(this.hor - hor) <= 1);
}

const Constructors = {"K": King, "R": Rook}

const read_notation = (str) => ({"color": str.charAt(0), "piece": str.charAt(1), "vert": str.charAt(2), "hor": str.charAt(3)});

function Move(piece, vert, hor) {
    this.piece = piece;
    this.vert = vert;
    this.hor = hor;
}

function Board(str, FirstPlayer, SecondPlayer) {
    this.pieces = str.split(/\s+/).reduce((stack, token) => {
        arr = read_notation(token);
    stack.push(new Constructors[arr.piece](arr.color, arr.vert, arr.hor));
    return stack;
}, []);
    this.firstPlayer = FirstPlayer;
    this.secondPlayer = SecondPlayer;
}

Board.prototype.check_end = function(turn) {
    return false;
    //ToDo
    // эта функция должна была проверять, мат ли на доске
}
Board.prototype.check_move = function (color, move) {
    return (color === move.piece.color &&
        this.pieces.find(move.piece) !== undefined &&
        move.piece.check_move(move.vert, move.hor));
}
Board.prototype.toPosition = function () {
    return Array.from(this.pieces);
}
Board.prototype.make_move = function (move) {
    this.pieces.map((piece) => {
        if (piece.toString() === move.piece) {
        piece.make_move(move.vert, move.hor);
    }
})
}
Board.prototype.play = function () {
    let b = true;
    while (!check_end()) {
        let move, color;
        if (b) {
            move = this.firstPlayer.make_move(this.toPosition());
            color = 'w';
        } else {
            move = this.secondPlayer.make_move(this.toPosition());
            color = 'b';
        }
        if (check_move(move, color)) {
            this.make_move(move);
        }
        b = !b;
    }
}

function PersonPlayer() {}
PersonPlayer.prototype.make_move = function (position) {
    //ToDo
    // эта функция должна была возвращать объект класса Move
    // т.е. ход игрока-человека, обрабатывая ввод-вывод из консоли
    return undefined;
}

function ComputerPlayer() {}
ComputerPlayer.prototype.make_move = function (position) {
    //ToDo
    // эта функция должна была возвращать объект класса Move
    // т.е. ход игрока-компьютера
    return undefined;
}

function play(str) {
    let board = new Board(str, new PersonPlayer(), new ComputerPlayer());
    board.play();
}

// положение фигур и ходы вводятся в формате:
// wKa8 = white King a8