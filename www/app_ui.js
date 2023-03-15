
// Trigger the click event on the default active tab after the Shiny app is loaded
// so that renderPlot or renderPlotly have the value of the selected tab on startup
$(document).on('shiny:connected', function () {
    $('#nav_hosp_plot_tab').trigger('click');
});

// Keep track of the current tab
document.getElementById('nav_vacc_map_tab').addEventListener('click', function () {
    Shiny.setInputValue('active_tab', 'nav_vacc_map_tab');
});

document.getElementById('nav_hosp_plot_tab').addEventListener('click', function () {
    Shiny.setInputValue('active_tab', 'nav_hosp_plot_tab');
});

document.getElementById('nav_about_tab').addEventListener('click', function () {
    Shiny.setInputValue('active_tab', 'nav_about_tab');
});