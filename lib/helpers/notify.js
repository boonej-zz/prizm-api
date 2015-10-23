var gcm = require('node-gcm');
var request = require('request');
var _ = require('underscore');

var pushURL = 'http://ec2-54-186-223-100.us-west-2.compute.amazonaws.com/push';

exports.sendNote = function(user, contents){
  console.log('received note');
  console.log(contents);
  if (user.device_token) {
    sendAPNMessage(user, contents);  
  }
  if (user.google_devices && user.google_devices.length > 0) {
    sendGCMMessage(user, contents);
  }
};

var sendGCMMessage = function(user, contents) {
  var message = new gcm.Message();
  message.addNotification({
    title: contents.title,
    body: contents.body,
    icon: contents.icon
  });
  if (contents.data) {
    message.addData(data);
  }
  sender = new gcm.sender(process.env.GOOGLE_PUSH_API_KEY);
  sender.send(message, {registrationIds: user.google_devices}, function(err, result){
    if (err) console.log(err);
    console.log(result);
  }); 
};

var sendAPNMessage = function(user, contents) {
  var body = {
    device: user.device_token,
    alert: contents.body,
    badge: user.badge_count
  };
  if (contents.data) {
    body.payload = contents.data;
  }
  request({
    url: pushUrl,
    method: 'POST',
    json: true,
    body: body
  }, function(err, result) {
    if (err) console.log(err);
    console.log(result);
  }); 
};
