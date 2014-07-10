process.env.PRISM_HOME = process.cwd() + '/';
var mongoose = require('mongoose');
var Post = require(process.env.PRISM_HOME + 'models/post').Post;
var User = require(process.env.PRISM_HOME + 'models/user').User;

mongoose.connect('mongodb://127.0.0.1/prism');

//User.find({type: {$in : ['institution_verified', 'institution']}}, function(err, users){
User.find({}, function(err, users){
  console.log(users);
  if(err) throw err;
    for(var i=0; i < users.length; i++){
      var user = users[i];
      //updatePostSubType(user, function(){});
      updateUserStatusToActive(user, function(){});

      if( i === users.length -1){
        unsetUserStatusProperty(function(){});
      }
    }
    
});

var updatePostSubType = function(user, block){
  Post.update({creator: user._id}, {$set: {subtype: user.subtype}}, {multi: true}, function(err, count){
    if(err) throw err;
    console.log('Updated #' + count + ' for user '+ user._id);
    block();
  });
};

var updateUserStatusToActive = function(user, block){
  if(user){
    if(user.status === 1){
      user.active = false;
    } else {
      user.active = true;
    }
    user.save(function(err, saved){
      if(err) throw err;
      if(saved) console.log("Successfully update user: "+user._id.toString());
      block();
    });
  } else {
    block();
  }
};

var unsetUserStatusProperty = function(block){
  User.update({}, {$unset: {status: ""}}, {multi: true}, function(err, result){
    if(err) throw err;
    console.log('Unset the "status" property in user collection docs');
  });
};
