'use strict';
const isExtraTaskSolved = true;

function toUTCTime(time) {
    const timezone = parseInt(time.split('+')[1]);
    const hour = parseInt(time.split('+')[0].split(' ')[1].split(':')[0]);
    const min = parseInt(time.split('+')[0].split(' ')[1].split(':')[1]);
    const day = time.split('+')[0].split(' ')[0];
    let dayN
    switch (day) {
        case 'ПН':
            dayN = 0;
            break;
        case 'ВТ':
            dayN = 1;
            break;
        case 'СР':
            dayN = 2;
            break;
        case 'ЧТ':
            dayN = 3;
            break;
        case 'ПТ':
            dayN = 4;
            break;
        case 'СБ':
            dayN = 5;
            break;
        case 'ВС':
            dayN = 6;
            break;
    }
    return ((24 * dayN + hour - timezone) * 60 + min);
}

function fromUTCTime(time) {
    const min = time % 60;
    const hour = ((time - min) / 60) % 24;
    const day = (time - min - 60 * hour) / (60 * 24);
    let dayN
    switch (day) {
        case 0:
            dayN = 'ПН';
            break;
        case 1:
            dayN = 'ВТ';
            break;
        case 2:
            dayN = 'СР';
            break;
        case 3:
            dayN = 'ЧТ';
            break;
        case 4:
            dayN = 'ПТ';
            break;
        case 5:
            dayN = 'СБ';
            break;
        case 6:
            dayN = 'ВС';
            break;
    }
    return {
        dayN,
        hour,
        min,
    };
}

function getBankWorkingTimes(bankWorkingHours, bankTimeZone) {
    return [
        {
            from: toUTCTime(`ПН ${bankWorkingHours.from}`) + 60 * bankTimeZone,
            to: toUTCTime(`ПН ${bankWorkingHours.to}`) + 60 * bankTimeZone
        },
        {
            from: toUTCTime(`ВТ ${bankWorkingHours.from}`) + 60 * bankTimeZone,
            to: toUTCTime(`ВТ ${bankWorkingHours.to}`) + 60 * bankTimeZone
        },
        {
            from: toUTCTime(`СР ${bankWorkingHours.from}`) + 60 * bankTimeZone,
            to: toUTCTime(`СР ${bankWorkingHours.to}`) + 60 * bankTimeZone
        }
    ];
}

function createPeriod(stringPeriod, bankTimeZone) {
    return {
        from: (toUTCTime(stringPeriod.from) + bankTimeZone * 60),
        to: (toUTCTime(stringPeriod.to) + bankTimeZone * 60)
    };
}

function getRobberyTimes(busyTimes, workingHours, timezone) {
    const Danny = invert(busyTimes.Danny, timezone);
    const Linus = invert(busyTimes.Linus, timezone);
    const Rusty = invert(busyTimes.Rusty, timezone);
    const robberyTimes = crossTimes(Danny, Linus, Rusty, workingHours);
    return robberyTimes;
}

function crossTimes(Danny, Linus, Rusty, workingHours) {
    let crossPer1 = [];
    Danny.forEach(period => {
        Linus.forEach(period1 => {
            crossPer1.push(getNewPer(period, period1));
        });
    });
    let crossPer2 = [];
    Rusty.forEach(period => {
        workingHours.forEach(period1 => {
            crossPer2.push(getNewPer(period, period1));
        });
    });
    crossPer1 = crossPer1.filter(data => data !== undefined);
    crossPer2 = crossPer2.filter(data => data !== undefined);
    let finalCross = [];
    crossPer1.forEach(period => {
        crossPer2.forEach(period1 => {
            finalCross.push(getNewPer(period, period1));
        });
    });
    return finalCross.filter(data => data !== undefined);
}

function getNewPer(period1, period2) {
    if (period1.to <= period2.from) {
        return undefined;
    }
    if (period1.from <= period2.from && period1.to >= period2.to) {
        return {from: Math.max(period1.from, period2.from), to: Math.min(period1.to, period2.to)}
    }
    if (period2.from <= period1.from && period2.to >= period1.to) {
        return {from: Math.max(period1.from, period2.from), to: Math.min(period1.to, period2.to)}
    }
    if (period2.from <= period1.to && period2.to >= period1.from) {
        return {from: Math.max(period1.from, period2.from), to: Math.min(period1.to, period2.to)}
    }


}

function invert(periods, timezone) {
    periods.sort((a, b) => a.from - b.to);
    let startTime = toUTCTime(`ПН 00:00+${timezone}`) + 60 * timezone;
    let finishTime = toUTCTime(`СР 23:59+${timezone}`) + 60 * timezone;
    let newPer = [];
    periods.forEach(period => {
        newPer.push({from: startTime, to: period.from});
        startTime = period.to;
    })
    newPer.push({from: startTime, to: finishTime});
    return newPer;
}

/**
 * @param {Object} schedule Расписание Банды
 * @param {number} duration Время на ограбление в минутах
 * @param {Object} workingHours Время работы банка
 * @param {string} workingHours.from Время открытия, например, "10:00+5"
 * @param {string} workingHours.to Время закрытия, например, "18:00+5"
 * @returns {Object}
 */
function getAppropriateMoment(schedule, duration, workingHours) {
    const timezone = parseInt(workingHours.from.split('+')[1]);
    const busyTimes = {
        Danny: schedule.Danny.map(x => createPeriod(x, timezone)),
        Linus: schedule.Linus.map(x => createPeriod(x, timezone)),
        Rusty: schedule.Rusty.map(x => createPeriod(x, timezone))
    };
    const bankTimes = getBankWorkingTimes(workingHours, timezone);
    let robberyTimes = getRobberyTimes(busyTimes, bankTimes, timezone);
    return {
        /**
         * Найдено ли время
         * @returns {boolean}
         */
        exists() {
            for (let i = 0; i < robberyTimes.length; i++) {
                if (robberyTimes[i].to - robberyTimes[i].from >= duration) {
                    return true;
                }
            }
            return false;
        },

        /**
         * Возвращает отформатированную строку с часами
         * для ограбления во временной зоне банка
         *
         * @param {string} template
         * @returns {string}
         *
         * @example
         * ```js
         * getAppropriateMoment(...).format('Начинаем в %HH:%MM (%DD)') // => Начинаем в 14:59 (СР)
         * ```
         */
        format(template) {
            robberyTimes = robberyTimes.filter(per => per.to - per.from >= duration);
            if (this.exists(robberyTimes) === false) {
                return '';
            } else {
                for (let i = 0; i < robberyTimes.length; i++) {
                    if (robberyTimes[i].to - robberyTimes[i].from >= duration) {
                        let myTime = fromUTCTime(robberyTimes[i].from);
                        if (myTime.min.toString().length === 1) {
                            myTime.min = `0${myTime.min}`;
                        }
                        if (myTime.hour.toString().length === 1) {
                            myTime.hour = `0${myTime.hour}`;
                        }
                        return template
                            .replace('%HH', `${myTime.hour}`)
                            .replace('%MM', `${myTime.min}`)
                            .replace('%DD', `${myTime.dayN}`);

                    }
                }
            }
        },

        /**
         * Попробовать найти часы для ограбления позже [*]
         * @note Не забудь при реализации выставить флаг `isExtraTaskSolved`
         * @returns {boolean}
         */
        tryLater() {
            robberyTimes = robberyTimes.filter(per => per.to - per.from >= duration);
            if (robberyTimes.length === 0) {
                return false;
            }
            if (robberyTimes[0].to - robberyTimes[0].from >= duration + 30) {
                robberyTimes[0].from += 30;
                return true;
            }
            if (robberyTimes.length > 1) {
                robberyTimes.shift()
                return true;
            }
            return false;
        }
    };
}

module.exports = {
    getAppropriateMoment
};