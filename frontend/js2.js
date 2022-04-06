'use strict';
/**
 * Телефонная книга
 */
const phoneBook = new Map();
const CREATE = 'Создай';
const CHEL = 'контакт';
const DEL = 'Удали';
const ADD = 'Добавь';
const TEL = 'телефон';
const AND = 'и';
const EM = 'почту';
const FOR = 'для';
const CHELA = 'контакта';
const SHOW = 'Покажи';
const EMS = 'почты';
const TELS = 'телефоны';
const CHELS = 'контактов,';
const WHE = 'где';
const IS = 'есть';
const CHELI = 'контакты,';
const NAME = 'имя';

/**
 * Вызывайте эту функцию, если есть синтаксическая ошибка в запросе
 * @param {number} lineNumber – номер строки с ошибкой
 * @param {number} charNumber – номер символа, с которого запрос стал ошибочным
 */
function syntaxError(lineNumber, charNumber) {
    throw new Error(`SyntaxError: Unexpected token at ${lineNumber}:${charNumber}`);
}


/**
 * Выполнение запроса на языке pbQL
 * @param {string} query
 * @returns {string[]} - строки с результатами запроса
 */


function run(query) {
    const lines = query.split(';')
    let answer = [];
    for (let i = 0; i < lines.length - 1; i++) {

        const query = lines[i].split(' ');
        if (query.length === 2) {
            syntaxError(i + 1, 15);
        }
        switch (query[0]) {
            case CREATE:
                switch (query[1]) {
                    case CHEL:
                        query.splice(0, 2)
                        let name = query.join(' ');
                        createContact(name);
                        break;
                    default:
                        syntaxError(i + 1, 8);
                }
                break;
            case DEL:
                switch (query[1]) {
                    case CHEL:
                        query.splice(0, 2);
                        let name = query.join(' ');
                        deleteContact(name);
                        break;
                    case TEL:
                    case EM:
                        let tels = [];
                        let emails = [];
                        let ind = 1;
                        while (true) {
                            let flag = false;
                            switch (query[ind]) {
                                case TEL:
                                    if (checkTel(query[ind + 1])) {
                                        tels.push(query[ind + 1]);
                                        ind += 2;
                                    } else {
                                        syntaxError(i + 1, query.slice(0, ind + 1).join(" ").length + 2);
                                    }
                                    break;
                                case EM:
                                    emails.push(query[ind + 1]);
                                    ind += 2;
                                    break;
                                case AND:
                                    ind++;
                                    break;
                                case FOR:
                                    flag = true;
                                    break;
                                default:
                                    syntaxError(i + 1, query.slice(0, ind).join(" ").length + 2);
                            }
                            if (flag) break;
                        }
                        ind++;
                        if (query[ind] === CHELA) {
                            delTelEmail(query.slice(ind + 1).join(" "), tels, emails);
                        } else {
                            syntaxError(i + 1, query.slice(0, ind).join(" ").length + 2);
                        }
                        break;
                    case CHELI:
                        switch (query[2]) {
                            case WHE:
                                switch (query[3]) {
                                    case IS:
                                        let zap = query.slice(4).join(" ");
                                        deleteContacts(zap);
                                        break;
                                    default:
                                        syntaxError(i + 1, 21);
                                }
                                break;
                            default:
                                syntaxError(i + 1, 17);
                        }
                        break;

                    default:
                        syntaxError(i + 1, 7);
                }
                break;
            case ADD:
                switch (query[1]) {
                    case TEL:
                    case EM:
                        let tels = [];
                        let emails = [];
                        let ind = 1;
                        while (true) {
                            let flag = false;
                            switch (query[ind]) {
                                case TEL:
                                    if (checkTel(query[ind + 1])) {
                                        tels.push(query[ind + 1]);
                                        ind += 2;
                                    } else {
                                        syntaxError(i + 1, query.slice(0, ind + 1).join(" ").length + 2);
                                    }
                                    break;
                                case EM:
                                    emails.push(query[ind + 1]);
                                    ind += 2;
                                    break;
                                case AND:
                                    ind++;
                                    break;
                                case FOR:
                                    flag = true;
                                    break;
                                default:
                                    syntaxError(i + 1, query.slice(0, ind).join(" ").length + 2);
                            }
                            if (flag) break;
                        }
                        ind++;
                        if (query[ind] === CHELA) {
                            addTelEm(query.slice(ind + 1).join(" "), tels, emails);
                        } else {
                            syntaxError(i + 1, query.slice(0, ind).join(" ").length + 2);
                        }
                        break;
                    default:
                        syntaxError(i + 1, 8);
                }
                break;
            case SHOW:
                switch (query[1]) {
                    case TELS:
                    case EMS:
                    case NAME:
                        let info = [];
                        let ind = 1;
                        while (true) {
                            let flag = false;
                            switch (query[ind]) {
                                case TELS:
                                    info.push("tel");
                                    ind++;
                                    break;
                                case EMS:
                                    info.push("email");
                                    ind++;
                                    break;
                                case NAME:
                                    info.push("name");
                                    ind++;
                                    break;
                                case AND:
                                    ind++;
                                    break;
                                case FOR:
                                    flag = true;
                                    break;
                                default:
                                    syntaxError(i + 1, query.slice(0, ind).join(" ").length + 2);
                            }
                            if (flag) break;
                        }
                        ind++;
                        if (query[ind] === CHELS) {
                            if (query[ind + 1] === WHE) {
                                if (query[ind + 2] === IS) {
                                    answer = answer.concat(showByQuery(query.slice(ind + 3).join(" "), info));
                                } else {
                                    syntaxError(i + 1, query.slice(0, ind + 2).join(" ").length + 2);
                                }
                            } else {
                                syntaxError(i + 1, query.slice(0, ind + 1).join(" ").length + 2);
                            }
                        } else {
                            syntaxError(i + 1, query.slice(0, ind).join(" ").length + 2);
                        }
                        break;
                    default:
                        syntaxError(i + 1, 8);
                }
                break;
            default:
                syntaxError(i + 1, 1);
        }
    }
    if (lines[lines.length - 1] !== "") {
        syntaxError(lines.length, lines[lines.length - 1].length + 1);
    }
    return answer;
}

function createContact(name) {
    if (phoneBook.has(name)) {
        return;
    }
    if (!phoneBook.has(name)) {
        phoneBook.set(name, {phones: [], emails: []})
    }
}

function delTelEmail(name, phones, emails) {
    if (!phoneBook.has(name)) {
        return;
    }
    phones.forEach(x => phoneBook.get(name).phones.splice(phoneBook.get(name).phones.indexOf(x), 1));
    emails.forEach(x => phoneBook.get(name).emails.splice(phoneBook.get(name).emails.indexOf(x), 1));
}

function deleteContact(name) {
    if (!phoneBook.has(name)) {
        return;
    }
    if (phoneBook.has(name)) {
        phoneBook.delete(name);
    }
}

function checkTel(tel) {
    return /^\d{10}$/.test(tel);
}

function deleteContacts(zap) {
    if (zap.length === 0) {
        return
    }
    const contacts = []
    phoneBook.forEach((pAndE, name) => {
        const inName = name.includes(zap)
        const inPhones = pAndE.phones.reduce((res, curr) => res || curr.includes(zap), false)
        const inEmails = pAndE.emails.reduce((res, curr) => res || curr.includes(zap), false)
        if (inName || inPhones || inEmails) {
            contacts.push(name)
        }
    })
    contacts.forEach(e => phoneBook.delete(e))
}

function addTelEm(name, tels, mails) {
    if (!phoneBook.has(name)) {
        return;
    }
    tels.forEach(phone => {
        if (phoneBook.get(name).phones.indexOf(phone) === -1) {
            phoneBook.get(name).phones.push(phone)
        }
    })
    mails.forEach(email => {
        if (phoneBook.get(name).emails.indexOf(email) === -1) {
            phoneBook.get(name).emails.push(email)
        }
    })
}

function formatTel(tel) {
    return "+7 (" + tel[0] + tel[1] + tel[2] + ") " + tel[3] + tel[4] + tel[5] + "-" + tel[6] + tel[7] + "-" + tel[8] + tel[9];
}

function showByQuery(s, info) {
    if (s === "") {
        return [];
    }
    if (!info.includes('email') && !info.includes('tel') && !info.includes('name')) {
        return []
    }
    const contacts = []
    phoneBook.forEach((pAndE, name) => {
        const inName = name.includes(s)
        const inPhones = pAndE.phones.reduce((res, curr) => res || curr.includes(s), false)
        const inEmails = pAndE.emails.reduce((res, curr) => res || curr.includes(s), false)
        if (inName || inPhones || inEmails) {
            contacts.push(name)
        }
    })
    const result = []
    contacts.forEach(name => {
        const answer = []
        info.forEach(token => {
            switch (token) {
                case 'name':
                    answer.push(name)
                    break
                case 'tel':
                    answer.push(phoneBook
                        .get(name)
                        .phones
                        .map(p => formatTel(p))
                        .join(','))
                    break
                case 'email':
                    answer.push(phoneBook.get(name).emails.join(','))
                    break
                default:
                    break
            }
        })
        result.push(answer.join(';'))
    })
    return result
}


module.exports = {phoneBook, run};