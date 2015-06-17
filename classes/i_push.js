var _request = require('request');

var pushURL = 'http://ec2-54-186-223-100.us-west-2.compute.amazonaws.com/push';

exports.sendNotification = function(contents, next){
  _request({
    url: pushURL,
    method: 'POST',
    json: true,
    body: contents  
  }, function(err, result){
    next(err, result);
  }
  );
};

