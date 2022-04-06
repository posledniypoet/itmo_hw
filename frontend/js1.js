'use strict';

/**
 * Складывает два целых числа
 * @param {Number} a Первое целое
 * @param {Number} b Второе целое
 * @throws {TypeError} Когда в аргументы переданы не числа
 * @returns {Number} Сумма аргументов
 */
function abProblem(a, b) {
    if (!Number.isInteger(a) || !Number.isInteger(b)){
        throw new TypeError("В аргументы переданы не числа");
    } else {
        return a + b;
    }
}

/**
 * Определяет век по году
 * @param {Number} year Год, целое положительное число
 * @throws {TypeError} Когда в качестве года передано не число
 * @throws {RangeError} Когда год – отрицательное значение
 * @returns {Number} Век, полученный из года
 */
function centuryByYearProblem(year) {
    if (!Number.isInteger(year)) {
        throw new TypeError("Передано не целое число")
    } else {
        if (year < 0) {
            throw new RangeError("Передано не положительное число")
        } else {
            if (year === 0) {
                return 0;
            } else {
                if (year % 100 === 0) {
                    return Math.trunc(year / 100);
                } else {
                    return Math.trunc(year / 100) + 1;
                }
            }
        }
    }
}

/**
 * Переводит цвет из формата HEX в формат RGB
 * @param {String} hexColor Цвет в формате HEX, например, '#FFFFFF'
 * @throws {TypeError} Когда цвет передан не строкой
 * @throws {RangeError} Когда значения цвета выходят за пределы допустимых
 * @returns {String} Цвет в формате RGB, например, '(255, 255, 255)'
 */
function colorsProblem(hexColor) {
    if (typeof hexColor !== 'string') {
        throw new TypeError("That's not a String");
    }
    if (!hexColor.match(/#[\da-fA-F][\da-fA-F][\da-fA-F][\da-fA-F][\da-fA-F][\da-fA-F]/) || !hexColor.match(/#[\da-fA-F][\da-fA-F][\da-fA-F]/)) {
        throw new RangeError(" That's not in range of HEX colors");

    }
    let rgb1;
    let rgb2;
    let rgb3;

    if(hexColor.length === 7){
        rgb1 = parseInt(hexColor.substring(1,3),16);
        rgb2 = parseInt(hexColor.substring(3,5),16);
        rgb3 = parseInt(hexColor.substring(5,7),16);
    } else {
        rgb1 = parseInt(hexColor[1]+hexColor[1],16);
        rgb2 = parseInt(hexColor[2]+hexColor[2],16);
        rgb3 = parseInt(hexColor[3]+hexColor[3],16);
    }
    return `(${rgb1}, ${rgb2}, ${rgb3})`;
}

/**
 * Находит n-ое число Фибоначчи
 * @param {Number} n Положение числа в ряде Фибоначчи
 * @throws {TypeError} Когда в качестве положения в ряде передано не число
 * @throws {RangeError} Когда положение в ряде не является целым положительным числом
 * @returns {Number} Число Фибоначчи, находящееся на n-ой позиции
 */
function fibonacciProblem(n) {
    if(typeof n !== 'number'){
        throw new TypeError("n isn't Number")
    }
    if(!Number.isInteger(n) || n <= 0){
        throw new RangeError("n > 0")
    }
    let fib1 = 0;
    let fib2 = 1;
    let temp;
    for (let i = 0; i < n;i++){
        temp = fib1 + fib2;
        fib1 = fib2;
        fib2 = temp;
    }
    return fib1;
}

/**
 * Транспонирует матрицу
 * @param {(Any[])[]} matrix Матрица размерности MxN
 * @throws {TypeError} Когда в функцию передаётся не двумерный массив
 * @returns {(Any[])[]} Транспонированная матрица размера NxM
 */
function matrixProblem(matrix) {
    if (!Array.isArray(matrix) || !matrix.every(Array.isArray)) {
        throw new TypeError("must be 2-dimensional array")
    } else {
        let N = matrix.length;
        let M = matrix[0].length;
        let trans = new Array(M);
        for (let i = 0; i < M; i++) {
            trans[i] = new Array(N);
        }
        for (let i = 0; i < M; i++) {
            for (let j = 0; j < N; j++) {
                trans[i][j] = matrix[j][i];
            }
        }
        return trans;
    }
}

/**
 * Переводит число в другую систему счисления
 * @param {Number} n Число для перевода в другую систему счисления
 * @param {Number} targetNs Система счисления, в которую нужно перевести (Число от 2 до 36)
 * @throws {TypeError} Когда переданы аргументы некорректного типа
 * @throws {RangeError} Когда система счисления выходит за пределы значений [2, 36]
 * @returns {String} Число n в системе счисления targetNs
 */
function numberSystemProblem(n, targetNs) {
    if (typeof n !== 'number' || typeof targetNs !== 'number'){
        throw new TypeError(" Must be numbers");
    } else {
        if (targetNs < 2 || targetNs > 36){
            throw new RangeError("TargetNs <36 and >2");
        } else{
            return n.toString(targetNs)
        }
    }
}

/**
 * Проверяет соответствие телефонного номера формату
 * @param {String} phoneNumber Номер телефона в формате '8–800–xxx–xx–xx'
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Boolean} Если соответствует формату, то true, а иначе false
 */
function phoneProblem(phoneNumber) {
    if(typeof phoneNumber !== 'string'){
        throw new TypeError("Requires String");
    } else {
        if(phoneNumber.match(/^8-800-\d\d\d-\d\d-\d\d$/)){
            return true;
        } else {
            return false;
        }
    }
}

/**
 * Определяет количество улыбающихся смайликов в строке
 * @param {String} text Строка в которой производится поиск
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Number} Количество улыбающихся смайликов в строке
 */
function smilesProblem(text) {
    if (typeof text !== 'string') {
        throw new TypeError("Requires String");
    }
    let arr = text.match(/:-\)|\(-:/g);
    if (arr === null) {
        return 0;
    } else {
        return arr.length;
    }
}

/**
 * Определяет победителя в игре "Крестики-нолики"
 * Тестами гарантируются корректные аргументы.
 * @param {(('x' | 'o')[])[]} field Игровое поле 3x3 завершённой игры
 * @returns {'x' | 'o' | 'draw'} Результат игры
 */
function ticTacToeProblem(field) {
    for (let i = 0; i < 3; i++) {
        if (field[i][0] === field[i][1]) {
            if (field[i][0] === field[i][2]) {
                return field[i][0];
            }
        }
    }
    for (let i = 0; i < 3; i++) {
        if (field[0][i] === field[1][i]) {
            if (field[0][i] === field[2][i]) {
                return field[0][i];
            }
        }
    }
    if(field[1][1] === field[2][2]){
        if(field[2][2] === field[0][0]){
            return field[1][1];
        }
    }
    if(field[1][1] === field[2][0]){
        if(field[1][1] === field[0][2]){
            return field[1][1];
        }
    }
    return 'draw';
}

module.exports = {
    abProblem,
    centuryByYearProblem,
    colorsProblem,
    fibonacciProblem,
    matrixProblem,
    numberSystemProblem,
    phoneProblem,
    smilesProblem,
    ticTacToeProblem
};