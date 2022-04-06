function httpPost(url, body) {
    fetch(url, {
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        method: "POST",
        body: JSON.stringify(body)
    })
        .then(response => response.json())
        .then(json => {
            drawCharts(json)
        });
}

function submitButton() {
    const formulaInput = document.getElementById("formula_input");
    const leftInput = document.getElementById("l_input");
    const rightInput = document.getElementById("r_input");
    const inaccuracyInput = document.getElementById("inaccuracy_input");
    const methodInput = document.getElementById("method_input");
    const params = {
        formula: formulaInput.value,
        l: leftInput.value,
        r: rightInput.value,
        inaccuracy: inaccuracyInput.value,
        method: methodInput.value
    }
    const str = window.location.origin + "/lab1/minimize"
    console.log(str)
    const url = new URL(str)
    httpPost(url, params)
}
