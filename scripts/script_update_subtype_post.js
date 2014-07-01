process.env.PRISM_HOME = process.cwd() + '/';
var mongoose = require('mongoose');
var Post = require(process.env.PRISM_HOME + 'models/post').Post;
var User = require(process.env.PRISM_HOME + 'models/user').User;

mongoose.connect('mongodb://127.0.0.1/prism');

User.find({type: {$in : ['institution_verified', 'institution']}}, function(err, users){
console.log(users);
  if(err) throw err;
    for(var i=0; i < users.length; i++){
      var user = users[i];
      updatePostSubType(user, function(){
	console.log('Logging this!');
});
      // Post.update({creator: user._id}, {$set: {subtype: user.subtype}}, {multi: true}, function(err, count){
      //   if(err) throw err;
      //   console.log('Updated #' + count + ' for user '+ user._id);
      // });
    }
});

var updatePostSubType = function(user, block){
  Post.update({creator: user._id}, {$set: {subtype: user.subtype}}, {multi: true}, function(err, count){
    if(err) throw err;
    console.log('Updated #' + count + ' for user '+ user._id);
    block();
  });
};

