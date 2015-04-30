var moment    = require('moment');
var _         = require('underscore');

moment.relativeTimeThreshold('d', 6);
moment.relativeTimeThreshold('M', 52);

moment.locale('en', {
  relativeTime: {
    past: "%s",
    s:  "%ds",
    m:  "%dm",
    mm: "%dm",
    h:  "%dh",
    hh: "%dh",
    d:  "%dd",
    dd: "%dd",
    M:  "4w",
    MM: function(number, withoutSuffix, key, isFuture){
      return number*4 + 'w';
    },
    y:  "%dy",
    yy: "%dy"
  } 
});

function timeSinceFormatter(date) {
  var now = moment();
  var create = moment(date);
  var diff = now.diff(create);
  diff = diff/1000;
  var timeSince = '';
  if (diff < 60) {
    timeSince = 'now';
  } if (diff < 3600) {
    var mins = Math.floor(diff / 60);
    timeSince = mins + 'm';
  } else if (diff < 60 * 60 * 24) {
    var hours = Math.floor(diff/(60*60));
    timeSince = hours + 'h';
  } else {
    var days = Math.floor(diff/(60*60*24));
    if (days < 7) {
      timeSince = days + 'd';
    } else {
      var weeks = Math.floor(days/7);
      timeSince = weeks + 'w';
    }
  }
  return timeSince;
}

exports.addTimeSinceFieldToObjects = function(objects) {
  function addTime(col){
    _.each(col, function(object) {
      if (object.create_date) {
        object.time_since = timeSinceFormatter(object.create_date);
      }
      if (object.comments) {
        addTime(object.comments);
      }
    });
    return col;
  }
  objects = addTime(objects);
  return objects;
};

exports.timeSinceFormatter = timeSinceFormatter;
