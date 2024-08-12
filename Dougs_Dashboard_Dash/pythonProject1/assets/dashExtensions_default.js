window.dashExtensions = Object.assign({}, window.dashExtensions, {
    default: {
        function0: function(feature, context) {
            const {
                superclass_colourv,
                circleOptions,
                colorProp
            } = context.hideout;
            style.fillColor = feature.properties[superclass_colourv]; // set color based on color prop
            return style; // render a simple circle marker
        }
    }
});