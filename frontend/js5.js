'use strict';


const friendsCompare = (first, second) => first.name.localeCompare(second.name);

function getInvitedFriends(allPeople, genderFilter, maxLevel = Infinity) {
    let circle = allPeople.filter(friend => friend.best).sort(friendsCompare);
    let guestList = [];
    while (maxLevel > 0 && circle.length > 0){
        guestList.push(...circle);
        circle = getNextCircle(circle, guestList, allPeople);
        maxLevel --;
    }
    return guestList.filter(friend => genderFilter.accum(friend));
}

function getNextCircle(circle, guestList, friends) {
    const newCircle = circle
        .reduce((result, person) => [...result, ...person.friends], [])
        .map(name => friends.find(friend => friend.name === name))
        .filter(friend => !guestList.includes(friend));

    return [...new Set(newCircle)].sort(friendsCompare);
}
/**
 * Итератор по друзьям
 * @constructor
 * @param {Object[]} friends
 * @param {Filter} filter
 */
function Iterator(friends, filter) {
    if (!(filter instanceof Filter)) {
        return TypeError("Wrong type");
    }
    this.listFriends = getInvitedFriends(friends, filter);
    this.cur = 0;
    this.done = function () {
        return this.listFriends.length === this.cur;
    }

    this.next = function () {
        if (this.done()) {
            return null;
        } else {
            return this.listFriends[this.cur++];
        }
    }
}

/**
 * Итератор по друзям с ограничением по кругу
 * @extends Iterator
 * @constructor
 * @param {Object[]} friends
 * @param {Filter} filter
 * @param {Number} maxLevel – максимальный круг друзей
 */
function LimitedIterator(friends, filter, maxLevel) {
    Iterator.call(this, friends, filter);
    this.listFriends = getInvitedFriends(friends, filter, maxLevel);
}

LimitedIterator.prototype = Object.create(Iterator.prototype);
LimitedIterator.prototype.constructor = LimitedIterator;

/**
 * Фильтр друзей
 * @constructor
 */
function Filter() {
    this.accum = () => true;
}

/**
 * Фильтр друзей
 * @extends Filter
 * @constructor
 */
function MaleFilter() {
    this.accum = friend => friend.gender === 'male';
}

MaleFilter.prototype = Object.create(Filter.prototype);
MaleFilter.prototype.constructor = MaleFilter;

/**
 * Фильтр друзей-девушек
 * @extends Filter
 * @constructor
 */
function FemaleFilter() {
    this.accum = friend => friend.gender === 'female';
}

FemaleFilter.prototype = Object.create(Filter.prototype);
FemaleFilter.prototype.constructor = FemaleFilter;


exports.Iterator = Iterator;
exports.LimitedIterator = LimitedIterator;

exports.Filter = Filter;
exports.MaleFilter = MaleFilter;
exports.FemaleFilter = FemaleFilter;