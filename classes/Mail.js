/**
 * Handles creating & sending email content/messaging
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _             = require('underscore'),
    _mailer       = require('nodemailer'),
    _home         = process.env.PRISM_HOME,
    _config       = require(_home + 'config/default'),
    PrismError    = require(_home + 'error'),
    _logger       = require(_home + 'logs'),
    User          = require(_home + 'models/user').User;

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
  this.transport = null;
  this.cb = callback;
  this.options = options;

  //setup transport
  self.__setupTransport();
}

/**
 * Sets up the SMTP transport config & conncetion
 *
 * @param n/a
 * @return n/a
 * @api private
 */
Mail.prototype.__setupTransport = function(){
  this.transport = _mailer.createTransport("SMTP", {
    host: _config.amazon.smtp.server,
    port: _config.amazon.smtp.port,
    ssl: true,
    auth: {
      user: _config.amazon.smtp.user,
      pass: _config.amazon.smtp.pass
    }
  });
};

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
  this.message = {
    from: 'dj.hayden@stablekernel.com',
    to: 'admin@prizmapp.com',
    subject: 'Prism Institution Review: ' + user.email,
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

  this.transport.sendMail(this.message, function(error){
    if(error){
      _logger.log("error","Error occured sending email", {error:error});
    }
    _logger.log('info', 'Message sent successfully');
  });
};

Mail.prototype.resetPassword = function resetPassword(user, block){
  return;
};
