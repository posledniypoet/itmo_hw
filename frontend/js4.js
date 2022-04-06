'use strict';

/**
 * Возвращает новый emitter
 * @returns {Object}
 */
function getEmitter() {
    const events = new Map();
    return {

        /**
         * Подписаться на событие
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         */
        on: function (event, context, handler) {
            if (events.has(event)) {
                events.get(event).push({context: context, handler: handler});
            } else {
                events.set(event, [{context: context, handler: handler}]);
            }
            return this;
        },

        /**
         * Отписаться от события
         * @param {String} event
         * @param {Object} context
         */
        off: function (event, context) {
            for (const key of events.keys()) {
                let event1 = event + ".";
                if (key === event || key.startsWith(event1)) {
                    events.set(key, events.get(key).filter(elem => elem.context !== context));
                }
            }
            return this;
        },

        /**
         * Уведомить о событии
         * @param {String} event
         */
        emit: function (event) {
            if (!events.has(event)) {
            } else {
                for (const {context, handler} of events.get(event)) {
                    handler.call(context);
                }
            }
            const splittedEvents = event.split('.');
            let allOfEvent = [];
            for (let i = 0; i < splittedEvents.length; i++) {
                let myEv = splittedEvents.slice(0, i + 1).join('.');
                allOfEvent.push(myEv);
            }
            for (let k = allOfEvent.length - 2; k >= 0; k--) {
                if (!events.has(allOfEvent[k])) {
                    return;
                }
                for (const {context, handler} of events.get(allOfEvent[k])) {
                    handler.call(context);
                }
            }
            return this;
        },

        /**
         * Подписаться на событие с ограничением по количеству полученных уведомлений
         * @star
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         * @param {Number} times – сколько раз получить уведомление
         */
        several: function (event, context, handler, times) {
            if (times <= 0) {
                return this.on(event, context, handler);
            }
            if (times > 0) {
                let ost = times;
                const newF = function () {
                    if (ost > 0) {
                        handler.call(context);
                        ost--;
                    }
                }
                return this.on(event, context, newF);
            }
        },

        /**
         * Подписаться на событие с ограничением по частоте получения уведомлений
         * @star
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         * @param {Number} frequency – как часто уведомлять
         */
        through: function (event, context, handler, frequency) {
            if (frequency <= 0) {
                return this.on(event, context, handler);
            }
            if (frequency > 0) {
                let ost = 0;
                const newF = function () {
                    if (ost >= 0) {
                        if (ost % frequency === 0) {
                            handler.call(context);
                        }
                        ost++;
                    }
                }
                return this.on(event, context, newF);
            }
        }
    };
}

module.exports = {
    getEmitter
};