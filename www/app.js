$(function() {
  // Correct initial state for buttons
  Shiny.addCustomMessageHandler('toggle-buttons', function(m) {
    for (let i = 0; i < m.length; i++) {
      let target = $('#edit-table').find('button')[i];
      $(target).prop('disabled', m[i]);
    }
  });

  Shiny.addCustomMessageHandler('close-modal-callback', function(m) {
    // Avoids to use timeout
    $(document).on('shown.bs.modal','#shiny-modal', function () {
      $('#shiny-modal').on('hide.bs.modal', function() {
        Shiny.setInputValue(`modal_${m}_closed`, true, {priority: 'event'});
      });
    });
  });

  Shiny.addCustomMessageHandler("can-save", function(m) {
    $('#edit-update_row').prop('disabled', !m);
  })
});
