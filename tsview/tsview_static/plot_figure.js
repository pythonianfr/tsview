class PlotFigure extends HTMLElement {
    static get observedAttributes() {
        return ['args', 'history-args', 'hover-args'];
    }

    attributeChangedCallback(name, old_value, new_value) {
        if ( name == 'args' ) {
            function doit() {
                let args = JSON.parse(new_value);
                Plotly.newPlot(
                    args.div,
                    args.data,
                    args.layout,
                    args.config
                );
                document.getElementById(args.div).on(
                    'plotly_relayout', function(eventData) {
                        let nullXRange = ["", ""];
                        let nullYRange = [Number.NaN, Number.NaN];
                        let panactive = false
                        if ('dragmode' in eventData) {
                            if (eventData['dragmode'] == 'pan') {
                                panactive = true;
                            };
                            app.ports.panActive.send(panactive);
                        };
                        if ('xaxis.autorange' in eventData &&
                            'yaxis.autorange' in eventData) {
                            app.ports.zoomPlot.send(
                                [nullXRange, nullYRange]
                            );
                        };
                        let xRange = nullXRange
                        let yRange = nullYRange
                        let send = false
                        if ('xaxis.range[0]' in eventData &&
                            'xaxis.range[1]' in eventData) {
                            xRange = [
                                eventData['xaxis.range[0]'],
                                eventData['xaxis.range[1]']
                            ];
                            send = true;
                        };
                        if ('yaxis.range[0]' in eventData &&
                            'yaxis.range[1]' in eventData) {
                            yRange = [
                                eventData['yaxis.range[0]'],
                                eventData['yaxis.range[1]']
                            ];
                            send = true;
                        };
                        console.log(eventData, xRange, yRange, send);
                        if (send) {
                            app.ports.zoomPlot.send(
                                [xRange, yRange]
                            );
                        };
                    }
                );
                document.getElementById(args.div).on(
                    'plotly_legendclick', function(legendData) {
                        let legend_status = [];
                        let status = true
                        let changed = legendData.curveNumber
                        // current status
                        for (var i=0; i < legendData.data.length; i++) {
                            let trace = legendData.data[i]
                            let name = trace.name;
                            if ("visible" in trace) {
                                if (trace["visible"] == "legendonly") {
                                    status = false;
                                }
                                else {
                                    status = true;
                                }
                            }
                            else {
                                status = true;
                            }
                            // change the status of the one clicked on
                            if (i == changed) {
                                status = !status
                            }
                            legend_status.push([name, status]);
                        }
                        app.ports.legendStatus.send(
                            legend_status
                        );
                    }
                );
            }
            setTimeout(doit, 1)
        }
        if ( name == 'history-args' ) {
            function doit () {
                let history_args = JSON.parse(new_value);
                Plotly.newPlot(
                    history_args.div,
                    history_args.data,
                    history_args.layout,
                );
                document.getElementById(history_args.div).on('plotly_hover', function(data) {
                    var points = data.points;
                    var list_data = [];
                    points.forEach(function(point){
                        list_data.push(
                            {
                                "date": point.data.name,
                                "value": point.y
                            }
                        );
                     });
                    var data = {
                        "name" : points[0].x,
                        "data" : list_data
                    };

                    app.ports.dataFromHover.send(JSON.stringify(data));
                });
             }
             setTimeout(doit, 10)
         }
         if ( name == 'hover-args' ) {
             function doit () {
                 let hover_args = JSON.parse(new_value);
                 Plotly.newPlot(
                     hover_args.div,
                     hover_args.data,
                     hover_args.layout,
                 );
             }
             setTimeout(doit, 10)
         }
    }
}
window.customElements.define("plot-figure", PlotFigure);
