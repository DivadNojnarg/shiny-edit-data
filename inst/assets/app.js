$(function() {
  // Needed to compute the diff for cell rendering.
  // This contains the very first data version.
  Shiny.addCustomMessageHandler('send-init-data', function(m) {
    initData = m.value;
  });
  // Correct initial state for buttons
  Shiny.addCustomMessageHandler('toggle-buttons', function(m) {
    for (let i = 0; i < m.value.length; i++) {
      let target = $('#edit-table').find('button')[i];
      $(target).prop('disabled', m.value[i]);
    }
  });

  // Similar to reactable::getReactableState for page

  Shiny.addCustomMessageHandler('register-pagination', function(m) {
    $('.rt-page-button').on('click', function() {
      Shiny.setInputValue(
        'current_page',
        parseInt($(this).html()),
        {priority: 'event'}
      );
    });
  });

  function isTableReady() {
   var tableReady = $('#edit-table > .Reactable').length === 1
   if (tableReady) {
     $(document).trigger('table-ready');
   }
  }

  let waitForTable = setInterval(isTableReady, 100);

  $(document).one('table-ready', function() {
    $('.rt-search').addClass('form-control form-control-lg border-1 border-secondary my-4');
    clearInterval(waitForTable);
  })

  // Go to previous pagination state when data are refreshed
  // This prevents pagination from being reset to 1.
  Shiny.addCustomMessageHandler('go-to-page', function(m) {
    setTimeout(function() {
      Reactable.gotoPage('edit-table', m.page - 1);
    }, 250);
  });

  Shiny.addCustomMessageHandler("can-save", function(m) {
    $('#edit-update_row').prop('disabled', !m.value);
  })
});
