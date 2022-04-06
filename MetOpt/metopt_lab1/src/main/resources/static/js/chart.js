Chart.defaults.global.legend.display = false

function drawCharts(json) {
    let datasets = []
    datasets.push({
        label: 'minimum',
        fill: false,
        pointRadius: 3.5,
        backgroundColor: 'rgb(255,41,41)',
        pointBorderColor: 'rgb(255,41,41)',
        pointBackgroundColor: 'rgb(255,41,41)',
        data: [{x: json.min.x, y: json.min.y}]
    })

    let data = new Array(json.function.length)
    for (let i = 0; i < data.length; i++) {
        data[i] = {x: json.function[i].x, y: json.function[i].y}
    }
    let graph = {
        fill: false,
        pointRadius: 0.4,
        backgroundColor: 'rgba(75,192,192,1)',
        pointBorderColor: 'rgba(75,192,192,1)',
        pointBackgroundColor: 'rgba(75,192,192,1)',
        data: data
    }
    datasets.push(graph)
    json.segments.forEach(val => {
        addSegment(datasets, val)
    })
    json.parabols.forEach(dots => {
        addParabola(datasets, dots, 0.2)
    })

    drawChart(json, 'divChart', 'myChart', datasets)

    const ctxMain = document.getElementById("charts");
    ctxMain.innerHTML = generateChartsHtml(json.iterations.length)

    for (let i = 0; i < json.iterations.length; i++) {
        let iter = json.iterations[i];
        let ds = []
        ds.push(graph)
        if (iter.type === 'SEGMENT') {
            addSegment(ds, iter.segment1)
            addSegment(ds, iter.segment2)
        } else if (iter.type === 'PARABOLA') {
            addParabola(ds, iter.parabola, 0.5)
        }
        drawChart(json, 'divChart' + i, 'myChart' + i, ds)
    }
}

function drawChart(json, divId, chartId, datasets) {
    const ctxMain = document.getElementById(divId);
    ctxMain.innerHTML = '<canvas id="' + chartId + '"></canvas>';
    const ctx = document.getElementById(chartId);

    const scatterChart = new Chart(ctx, {
        type: 'scatter',
        data: {
            datasets: datasets
        },
        options: {
            scales: {
                xAxes: [{
                    type: 'linear',
                    position: 'bottom',
                    backgroundColor: 'rgba(75,192,192,1)'
                }]
            }
        }
    });

    writeResult(json.min)
}

function addParabola(datasets, dots, width) {
    let data = new Array(dots.length)
    for (let i = 0; i < data.length; i++) {
        data[i] = {x: dots[i].x, y: dots[i].y}
    }

    datasets.push({
        label: 'parabols',
        fill: false,
        pointRadius: width,
        borderWidth: width,
        backgroundColor: 'rgba(255,255,255,1)',
        pointBorderColor: 'rgb(255,118,23)',
        pointBackgroundColor: '#fff',
        data: data
    })
}

function addSegment(datasets, val) {
    datasets.push({
        label: 'dots',
        fill: false,
        pointRadius: 0,
        borderWidth: 0.5,
        backgroundColor: 'rgba(34,102,192,0)',
        pointBorderColor: 'rgba(34,102,192,0)',
        pointBackgroundColor: 'rgba(0,0,0,0)',
        borderColor: '#808080',
        data: [{x: val.fst.x, y: val.fst.y}, {x: val.snd.x, y: val.snd.y}]
    })
}

function generateChartsHtml(length) {
    let str = ''
    for (let i = 0; i < length; i++) {
        str += '    <div id="divChart' + i + '" class="chart">\n' +
            '        <canvas id="myChart' + i + '"></canvas>\n' +
            '    </div>\n'
    }
    return str
}

function writeResult(min) {
    const ctx = document.getElementById("result");
    ctx.innerHTML = '<label class="min_label">Minimum dote:</label>\n' +
        '            <text class="min_result">(' + min.x + ', ' + min.y + ')</text>'
}
