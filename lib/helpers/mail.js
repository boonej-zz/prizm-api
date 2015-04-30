var mongoose 		= require('mongoose');
var config 			= require('../../config');
var fs 					= require('fs');
var path 				= require('path');
var jade 				= require('jade');
var insightMail = fs.readFileSync(path.join(__dirname +
      						'/../../views/mail/insight_email.jade'), 'utf8');
var welcomeMail = fs.readFileSync(path.join(__dirname + 
                  '/../../views/mail/welcome_mail.jade'), 'utf8');
var rejectMail  = fs.readFileSync(path.join(__dirname +
                  '/../../views/mail/reject_mail.jade'), 'utf8');
var acceptMail  = fs.readFileSync(path.join(__dirname +
                  '/../../views/mail/accept_mail.jade'), 'utf8');
var Insight 		= mongoose.model('Insight');
var User 				= mongoose.model('User');
var ObjectId 		= require('mongoose').Types.ObjectId;
var mandrill 		= require('node-mandrill')(config.mandrill.client_secret);
var mandrillEndpointSend = '/messages/send';

var Mixpanel = require('mixpanel');
var mixpanel = Mixpanel.init('831cd11c7aedb1175765d2a9583c5cff');

// var kADMIN = 'admin@prizmapp.com';
var kADMIN = 'eric@higheraltitude.co';

var emailSubjects = ['New Insight from Prizm',
                    'Oh, this? It’s for you.',
                    'Awesome stuff for you!',
                    'Cool stuff alert!',
                    'Did you see this?',
                    'Special delivery!',
                    'You gotta see this!',
                    'Happy Holidays from Prizm!',
                    'Happy New Year from Prizm!',
                    'Happy Valentine’s Day from Prizm!',
                    'Good Luck JoJo!'];

exports.unsubscribeUserByEmail = function(email, callback) {
	var distinctId = '';
  User.findOneAndUpdate({email: email}, {unsubscribed: true}, function(err, user) {
  	if (err) {
  		console.log(err);
  		callback(err, null);
  	}
  	if (user) {
      // distinctId = user._id;
      // mixpanel.people.set(distinctId, {
      //   '$unsubscribed': true
      // })
		  console.log("user was unsubscribed");
      callback(null, user);
  	}
  });
}

exports.subscribeUserByEmail = function(email, callback) {
	User.findOneAndUpdate({email: email}, {unsubscribed: false}, function(err, user){
		if (err) {
  		console.log(err);
  		callback(err, null);
  	}
  	if (user) {
  		console.log('user was subscribed');
      callback(null, user);
  	}
	})
}

exports.sendInsightEmail = function(insightTarget, index, optionalSubject) {
  var creatorId = insightTarget.creator;
  var targetId 	= insightTarget.target;
  var insightId = insightTarget.insight;
  var html = '';
  var subject = optionalSubject || emailSubjects[index];
  console.log(subject);
  Insight.findOne({_id: ObjectId(insightId)}, function (err, insight) {
    if (err) {
      console.log(err);
    };
    if (insight) {
      User.findOne({_id: ObjectId(creatorId)}, function (err, creator) {
        if (err) console.log(err);
        if (creator) {
          User.findOne({_id: ObjectId(targetId)}, function (err, target) {
            if (err) console.log(err);
            if (target) {
              if (target.unsubscribed == false) {
                html = jade.render(insightMail, {
                  creator: creator,
                  target: target,
                  insight: insight
                });
                mandrill(mandrillEndpointSend, {
                  message: {to: [{email: target.email}],
                  from_name: 'Prizm',
                  from_email: 'insight@prizmapp.com',
                  subject: subject,
                  html: html}
                }, function (err, response) {
                  if (err) {
                    console.log('MANDRILL ERROR RETURNED: ' + 
                      JSON.stringify(err));
                  };
                  if (response) {
                    console.log(JSON.stringify(response));
                  }
                });
              } else {
                console.log("User has unsubscribed");
              }
            }
          });
        };
      });
    };
  });
};

exports.sendNewPartnerMail = function(partner) {
  var message = {
    from_email: kADMIN,
    to: [{email: kADMIN}],
    subject: 'Prizm Institution Review: ' + partner.email,
    html: '<h1>Institution User Review</h1>'+
          '<h2>'+partner.email+'</h2><br>'+
          '<p>Name: '+partner.first_name+'</p>'+
          '<p>Email: '+partner.email+'</p>'+
          '<p>Phone Number: '+partner.phone_number+'</p>'+
          '<p>Website: '+partner.website+'</p><br>'+
          '<h1><a href="http://www.prizmapp.com/users/'+
          partner._id+'/institutions?review_key='+partner.review_key+'&approval=yes">Approve</a></h1>'+
          '<h1><a href="http://www.prizmapp.com/users/'+
          partner._id+'/institutions?review_key='+partner.review_key+'&approval=no">Deny</a></h1>'
  };

  mandrill(mandrillEndpointSend, {
    message: message
  }, function(err, res) {
    if (err) {
      console.log('Error sending new partner email: ' + err);
    }
    else {
      console.log('new partner email sent');
    }
  });
};

exports.sendWelcomeMail = function(user) {
  console.log('Sending email to ' + user.email);
  var html = jade.render(welcomeMail, {user: user});
  mandrill(mandrillEndpointSend, {
    message: {
      to: [{email: user.email}],
      from_email: 'info@prizmapp.com',
      from_name: 'Prizm',
      subject: 'Welcome to Prizm!',
      html: html
    }
  }, function (err, res) {
    if (err) {
      console.log('Error sending welcome email: ' + err);
    } else {
      console.log('welcome email sent to ' + user.email);
    }
  });
};

module.exports.emailSubjects = emailSubjects;
