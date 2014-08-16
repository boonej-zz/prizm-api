/**
 * Handles creating & sending email content/messaging
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _             = require('underscore'),
    _home         = process.env.PRISM_HOME,
    _config       = require('config'),
    _mailer       = require('node-mandrill')(_config.mandrill.client_secret),
    PrismError    = require(_home + 'error'),
    _logger       = require(_home + 'logs'),
    User          = require(_home + 'models/user').User;

var SEND_MESSAGE_ENDPOINT = '/messages/send';
var DEV_EC2_URI = 'https://ec2-user@ec2-54-186-28-238.us-west-2.compute.amazonaws.com';
var STAGING_EC2_URI = 'https://https://ec2-54-200-41-62.us-west-2.compute.amazonaws.com';

/**
 * Expose Module
 */
module.exports = Mail;

function Mail(options, callback){
  var self = this;
  this.to = null;
  this.from = null;
  this.subject = null;
  this.message = null;
  this.body = null;
  this.type = null;
  this.cb = callback;
  this.options = options;
}

/**
 * Sends a review email to admin when a user tries to register as a institution
 *
 * @param {Object} user The user object under review (details to be passed in html)
 * @param {Function} block The callback block to be invoked on run completion
 * @api public
 */
Mail.prototype.institutionReview = function institutionReview(user, block){
  if(!user){
    throw new Error('A user must be passed to Mail.institutionReview');
  }

  if(!_.has(user, 'email')){
    throw new Error('A user must have a valid email to send for review');
  }
  var approval_link = null;
  var denial_link = null;

  //create the message
  var admin = 'admin@prizmapp.com';

  this.message = {
    from_email: admin,
    to: [{email: admin}],
    subject: 'Prizm Institution Review: ' + user.email,
    html: '<h1>Institution User Review</h1>'+
          '<h2>'+user.email+'</h2><br>'+
          '<p>Name: '+user.first_name+'</p>'+
          '<p>Email: '+user.email+'</p>'+
          '<p>Phone Number: '+user.phone_number+'</p>'+
          '<p>Website: '+user.website+'</p><br>'+
          '<h1><a href="https://ec2-user@ec2-54-200-41-62.us-west-2.compute.amazonaws.com/users/'+
          user._id+'/review/institution?review_key='+user.review_key+'&approval=yes">Approve</a></h1>'+
          '<h1><a href="https://ec2-user@ec2-54-200-41-62.us-west-2.compute.amazonaws.com/users/'+
          user._id+'/review/institution?review_key='+user.review_key+'&approval=no">Deny</a></h1>'
  };

  _logger.log('info', 'Institution Review Message', {message: this.message});

  _mailer(SEND_MESSAGE_ENDPOINT, {message: this.message} , function(error, response){
    if(error){
      console.log("Error occured sending email" + JSON.stringify(error));
    }else{
      console.log('Message sent successfully ' + JSON.stringify(response));
    }
  });
};

Mail.prototype.resetPassword = function resetPassword(user, block){
  var approval_link = null;
  var denial_link = null;

  //create the message
  this.message = {
    from_email: 'admin@prizmapp.com',
    to: [{email: user.email}],
    subject: 'Prizm Password Reset for ' + user.email,
    html: '<h1><a href="http://www.prizmapp.com/users/'+user._id+'/password?reset_key='+
      user.reset_key+'&approval=yes">Confirm Password Reset</a></h1>'
  };

  _logger.log('info', 'reset passwrod email', {message: this.message});

  _mailer(SEND_MESSAGE_ENDPOINT, {message: this.message}, function(error, response){
    if(error) console.log('Error occured during password reste eamil');
    else console.log('successful passwordreset email: ' + JSON.stringify(response));
    if(block) block(error, response);
  });
};
