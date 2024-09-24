const cb_input_ignore = ['.vscomp-search-input'];

const get_filter = function(step_id, filter_id, ns_prefix) {
  var selector = '#' + ns_prefix + step_id + ' div.cb_filter[data-filter_id="' + filter_id +'"]';
  return $(selector);
};

const show_alert = function(message) {
  alert(message.info);
};
Shiny.addCustomMessageHandler('show_alert', show_alert);

const pre_add_step_action = function(message) {
  $('#' + message.ns_prefix + 'cb_container')
    .find('div.cb_step .cb_rm_step')
    .prop('disabled', true);
  $('#' + message.ns_prefix + 'cb_panel')
    .find('.cb_add_step')
    .blur();
};
Shiny.addCustomMessageHandler('pre_add_step_action', pre_add_step_action);

const post_rm_step_action = function(message) {
  $('#' + message.ns_prefix + 'cb_container')
    .find('div.cb_step:last-child:not(:first-child) .cb_rm_step')
    .prop('disabled', false);
};
Shiny.addCustomMessageHandler('post_rm_step_action', post_rm_step_action);

const inform_data_updated = function(message) {
  Shiny.setInputValue(message.ns_prefix + 'cb_data_updated', message, {priority: 'event'});
};
Shiny.addCustomMessageHandler('inform_data_updated', inform_data_updated);

const update_na_count = function(message) {
  $filter = get_filter(message.step_id, message.filter_id, message.ns_prefix);
  $filter.find('.cb_na_value').html(message.na_count);
};
Shiny.addCustomMessageHandler('update_na_count', update_na_count);

const up_state = function(message) {
  Shiny.setInputValue(message.ns_prefix + 'action', message, {priority: 'event'});
};
Shiny.addCustomMessageHandler('up_state', up_state);

const update_filter_class = function(message) {
  var step_selector = '#' + message.ns_prefix + message.step_id;
  var filter_content = $(step_selector)
    .find('div.cb_filter[data-filter_id="' + message.filter_id +'"] ' + '.cb_filter_content');
  if (message.show) {
    filter_content.removeClass(message.class);
  } else {
    filter_content.addClass(message.class);
  }
};
Shiny.addCustomMessageHandler('update_filter_class', update_filter_class);

const enable_panel = function(message) {
  $('#' + message.ns_prefix + 'cb_panel').addClass('disabled');
  if (message.enable === true) {
    $('#' + message.ns_prefix + 'cb_panel').removeClass('disabled');
  }
};
Shiny.addCustomMessageHandler('enable_panel', enable_panel);

const update_class = function(message) {
  var disabled;
  if (message.action == 'add') {
    $('#' + message.ns_prefix + message.step_id).addClass(message.class);
    disabled = false;
  }
  if (message.action == 'remove') {
    $('#' + message.ns_prefix + message.step_id).removeClass(message.class);
    disabled = true;
  }
  if (message.hasOwnProperty('disable')) {
    $('#' + message.ns_prefix + message.step_id)
      .find(message.disable)
      .prop('disabled', disabled);
  }
};
Shiny.addCustomMessageHandler('update_class', update_class);

var exec_event = {};
$(document).on('shiny:inputchanged', function(event) {
  var event_el = event.target;
  if (event_el == document) {
    return true;
  }
  var ignore_input = Boolean(event_el.dataset.ignore) || $(event_el).is(cb_input_ignore.join(','));
  if (ignore_input) {
    return true;
  }
  var filter_input = event_el.closest('.cb_input');
  if (Boolean(filter_input)) {
    var ns_prefix = filter_input.closest('.cb_container').dataset.ns_prefix;
    var step_id = filter_input.closest('.cb_step')?.dataset?.step_id;
    var filter_id = filter_input.closest('.cb_filter')?.dataset?.filter_id;
    var is_init = Boolean(filter_input.dataset.exec_state == 'init');
    var input_changed;
    var input_name = filter_input.dataset.param;
    var priority = filter_input.attributes.priority?.value;
    if (exec_event[event.name] == '_update-mode_' || is_init) {
      exec_event[event.name] = JSON.parse(JSON.stringify(event.value));
      filter_input.dataset.exec_state = 'active';
    } else if (JSON.stringify(exec_event[event.name]) != JSON.stringify(event.value) || priority === "event") {
      var input_value = JSON.parse(JSON.stringify(event.value));
      exec_event[event.name] = input_value;
      Shiny.setInputValue(
        ns_prefix + 'action',
        {
          id: 'update_filter',
          params: {
            step_id: step_id, filter_id: filter_id, input_value: input_value,
            input_name: input_name, binding: event.inputType, run_flow: false
          }
        },
        {priority: 'event'}
      );
    }
  }
});

$(document).on('shiny:updateinput', function(event) {
  if (Boolean(event.target.closest('.cb_input'))) {
    exec_event[event.target.id] = "_update-mode_";
  }
});
