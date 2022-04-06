'use strict';

global.fetch = require('node-fetch');

const API_KEY = require('./key.json');
/**
 * @typedef {object} TripItem Город, который является частью маршрута.
 * @property {number} geoid Идентификатор города
 * @property {number} day Порядковое число дня маршрута
 */

class TripBuilder {
    constructor(geoids) {
        this.geoids = geoids;
        this.conditions = [];
        this.duration = Infinity;
    }

    /**
     * Метод, добавляющий условие наличия в маршруте
     * указанного количества солнечных дней
     * Согласно API Яндекс.Погоды, к солнечным дням
     * можно приравнять следующие значения `condition`:
     * * `clear`;
     * * `partly-cloudy`.
     * @param {number} daysCount количество дней
     * @returns {object} Объект планировщика маршрута
     */
    sunny(daysCount) {
        for (let i = 0; i < daysCount; i++) {
            this.conditions.push('sunny');
        }

        return this;
    }

    /**
     * Метод, добавляющий условие наличия в маршруте
     * указанного количества пасмурных дней
     * Согласно API Яндекс.Погоды, к солнечным дням
     * можно приравнять следующие значения `condition`:
     * * `cloudy`;
     * * `overcast`.
     * @param {number} daysCount количество дней
     * @returns {object} Объект планировщика маршрута
     */
    cloudy(daysCount) {
        for (let i = 0; i < daysCount; i++) {
            this.conditions.push('cloudy');
        }

        return this;
    }

    /**
     * Метод, добавляющий условие максимального количества дней.
     * @param {number} daysCount количество дней
     * @returns {object} Объект планировщика маршрута
     */
    max(daysCount) {
        this.duration = daysCount;
        return this;
    }

    /**
     * Метод, возвращающий Promise с планируемым маршрутом.
     * @returns {Promise<TripItem[]>} Список городов маршрута
     */
    build() {
        return Promise.all(this.geoids.map(getConds)).then(cities => {
                const path = this.conditions.length;

                const closeCities = [];
                const resultTrip = [];
                let currentDay = 0;
                let update = true;

                while (update && currentDay < path) {
                    update = false;
                    cities.forEach(city => {
                        if (!closeCities.includes(city.id)) {
                            for (let i = 0; i < this.duration; i++) {
                                if (
                                    currentDay !== path &&
                                    city.conditions[currentDay] === this.conditions[currentDay]
                                ) {
                                    if (!update) {
                                        closeCities.push(city.id);
                                    }
                                    update = true;
                                    currentDay++;

                                    resultTrip.push({geoid: city.id, day: currentDay});
                                } else {
                                    break;
                                }
                            }
                        }
                    });
                }

                if (currentDay !== path) {
                    throw new Error('Не могу построить маршрут!');
                }

                return resultTrip;
            }
        )
    }
}

/**
 * Фабрика для получения планировщика маршрута.
 * Принимает на вход список идентификаторов городов, а
 * возвращает планировщик маршрута по данным городам.
 *
 * @param {number[]} geoids Список идентификаторов городов
 * @returns {TripBuilder} Объект планировщика маршрута
 * @see https://yandex.ru/dev/xml/doc/dg/reference/regions-docpage/
 */
function planTrip(geoids) {
    return new TripBuilder(geoids);
}

function getConds(geoid) {
    return global
        .fetch(`https://api.weather.yandex.ru/v2/forecast?hours=false&geoid=${geoid}&limit=7`, {
            headers: {
                "X-Yandex-API-Key": API_KEY.key
            }
        })
        .then(response => response.json())
        .then(json => ({
            id: geoid,
            conditions: json['forecasts'].map(day => (day['parts']['day_short']['condition'] === 'partly-cloudy' || day['parts']['day_short']['condition'] === 'clear') ? 'sunny' :
                (day['parts']['day_short']['condition'] === 'overcast' || day['parts']['day_short']['condition'] === 'cloudy') ? 'cloudy' : 'bad')
        }));
}

module.exports = {
    planTrip
};

