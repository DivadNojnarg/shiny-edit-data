$(function() {
  // Correct initial state for buttons
  Shiny.addCustomMessageHandler('toggle-buttons', function(m) {
    setTimeout(function() {
      for (let i = 0; i < m.length; i++) {
        let target = $('#edit-table').find('button')[i];
        if (m[i]) {
          $(target).prop('disabled', true);
        } else {
          $(target).prop('disabled', false);
        }
      }
    }, 500);
  });

  Shiny.addCustomMessageHandler('close-modal-callback', function(m) {
    setTimeout(function() {
      $('#shiny-modal').on('hidden.bs.modal', function() {
        Shiny.setInputValue(`modal_${m}_closed`, true, {priority: 'event'});
      });
    }, 2000);
  });
});
