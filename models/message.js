var mongoose = require('mongoose');
var ObjectId = mongoose.Schema.Types.ObjectId;
var time       = require('../lib/helpers/date_time');
var _ = require('underscore');


var messageSchema = new mongoose.Schema({
  creator: {type: ObjectId, ref: 'User', required: true},
  create_date: {type: Date, default: null, required: false, index: true},
  modify_date: {type: Date, default: null, required: false, index: true},
  text: {type: String, default: null},
  group: {type: ObjectId, index: true},
  organization: {type: ObjectId, ref: 'Organization', required: true, index: true},
  likes: {type: Array},
  likes_count: {type: Number, default: 0},
  pretty_text: {type: String},
  image_url: {type: String},
  target: {type: ObjectId, ref: 'User'},
  meta: {
    message_id: {type: ObjectId, ref:'Message'},
    description: {type: String},
    title: {type: String},
    url: {type: String},
    video_url: {type: String},
    image: {
      message_id: {type: ObjectId, ref: 'Message'},
      url:  {type: String},
      width:  {type: Number},
      height: {type: Number}
    }
  },
  read: {type: Array}
});

messageSchema.pre('save', function(next){
  if (!this.create_date){
    this.create_date = Date.now();
  }
  this.modify_date = Date.now();
  next();
});

messageSchema.post('init', function(){
  this.timeSince = time.timeSinceFormatter(this.create_date);
  if (this.meta) {
    this.meta.message_id = this._id;
    this.meta.image.message_id = this._id;
  }
  if (!this.read) {
    this.read = [];
  }
});

messageSchema.statics.fetchMessages = function(criteria, next){
  this.model('Message').find(criteria)
  .sort({create_date: -1})
  .populate({
    path: 'creator',
    select: '_id name profile_photo_url'
  })
  .limit(15)
  .exec(function(err, messages){
    next(err, messages);
  });
};

messageSchema.statics.likeMessage = function(id, user, next){
  this.model('Message').findOne({_id: id}, function(err, message){
    if (err) next(err, false);
    if (message) {
      console.log(message);
      if (!message.likes) message.likes = [];
      message.likes.push(user._id);
      message.likes_count += 1;
      message.save(function(e, r){
        next(e,r);    
      });
    } else {
      next(true, false);
    }
  });
};

messageSchema.statics.unlikeMessage = function(id, user, next){
  this.model('Message').findOne({_id: id}, function(err, message){
    if (err) next(err, false);
    if (message) {
      if (message.likes) {
        var idx = false;
        _.each(message.likes, function(p, i, l){
          if (String(p) == String(user._id)){
            idx = i;
          }
        });
        message.likes.splice(idx, 1);
        message.likes_count -= 1;
        message.save(function(e, r){
          next(e,r);    
        });
      } else {
        next(true, false);
      } 
     
    } 
  });
};

messageSchema.methods.prettyText = function(next) {
  var User = mongoose.model('User');
  var $this = this;
  User.resolvePostTags(this, function(err, users){
    var prettyText = $this.text;
    if ($this.text) {
      var match = $this.text.match(/@\S{24}/g);
      if (match && match.length > 0) {
        _.each(match, function(tag, idx, list){
          var uid = tag.substr(1);
          var mu = _.find(users, function(user){
            return String(user._id) == String(uid);
          });
          if (mu){
            prettyText = prettyText.replace(tag, '@' + mu.name);
          }
        });
      } 
    }
    next(prettyText);
  });
};



messageSchema.statics.findAndFlatten = function(criteria, requestor, limit, next){
  var model = this.model('Message');
  model.find(criteria)
  .sort({create_date: -1})
  .limit(limit || 25)
  .exec(function(err, messages) {
    if (messages && messages.length > 0) {
      model.populate(messages, {path: 'creator', 
        model: 'User', 
        select: {name: 1, profile_photo_url: 1, active: 1, subtype: 1}}, 
        function(err, messages){
          androidText(messages, requestor, function(result){
            result.reverse();
            next(err, result); 
          });
        });
    } else {
      next(err, []);
    }
  });
};

messageSchema.statics.createMessage = function(params, next) {
  var model = this.model('Message');
  var message = new model(params);
  message.save(function(err, result){
    model.populate(result, {path: 'creator', 
      model: 'User', 
      select: {name: 1, profile_photo_url: 1, active: 1, subtype: 1}}, 
      function(err, message) {
        androidText(message, params.creator, function(m){
          next(err, m);
        });
      });
  });
};

var extractTags = function(obj, users) {
  var at = obj.text;
  if (obj.text) {
    var match = String(obj.text).match(/@\w{24}/g);
    if (match && match.length > 0) {
      _.each(match, function(tag, idx, list){
        var uid = tag.substr(1);
        var mu = _.find(users, function(user){
          return String(user._id) == String(uid);
        });
        if (mu){
          at  = at.replace(tag, '@(' + mu.name + '|' + mu._id + ')');
        }
      });
    } 
  }
  return at;
}

var androidText = function(obj, requestor, next) {
  var User = mongoose.model('User');
  if (_.isArray(obj)) {
    var finalArray = [];
    _.each(obj, function(o, idx, list){
      messageLiked(o, requestor);
      User.resolvePostTags(o, function(err, users) {
        var at = extractTags(o, users);
        o = fillMessage(o);
        o.android_text = at;
        finalArray.push(o);
        if (finalArray.length == list.length) {
          next(finalArray);
        }
      });
    });
  } else {
    User.resolvePostTags(obj, function(err, users) {
      var at = extractTags(obj, users);   
      obj = fillMessage(obj);
      obj.android_text = at;
      next(obj);
    });

  }
}


var fillMessage = function(m) {
  m = m.toObject();
  m.creator_profile_photo_url = m.creator.profile_photo_url;
  m.creator_id = m.creator._id;
  m.creator_subtype = m.creator.subtype;
  m.creator_active = m.creator.active;
  m.creator_name = m.creator.name;
  if (m.meta) {
    if (m.meta.image && m.meta.image.url) {
      m.meta_image_url = m.meta.image.url;
    }
    if (m.meta.title) {
      m.meta_title = m.meta.title;
    }
    if (m.meta.description) {
      m.meta_description = m.meta.description;
    }
    if (m.meta.url) {
      m.meta_url = m.meta.url;
    }
    if (m.meta.video_url) {
      m.meta_video_url = m.meta.video_url;
    }
  }
  return m;
}

var messageLiked = function(m, r) {
  var isLiked = false;
  if (String(m.creator_id) == String(r)){
    m.my_post = true;
  } else {
    m.my_post = false;
  }
  _.each(m.likes, function(l){
    if (String(l) == String(r)) {
      isLiked = true;
    }
  });
  m.liked = isLiked;
};

messageSchema.statics.isLiked = function(messages, requestor) {
  _.each(messages, function(m){
    m = messageLiked(m, requestor);   
  });
  return messages;
}

messageSchema.statics.likeMessage = function(mid, uid, next){
  var model = this.model("Message");
  model.findOne({_id: mid}, function(err, message){
    if (err) next(err, message);
    else if (message) {
      var liked = false;
      var idx = -1;
      _.each(message.likes, function(l, index) {
          if (String(l) == String(uid)){
            liked = true;
            idx = index;
          }
      });
      if (liked) {
        message.likes.splice(idx, 1);
        message.likes_count -= 1;
      } else {
        message.likes.push(uid);
        message.likes_count +=1;
      }
      message.save(function(err, result) {
        if (err) next(err, result);
        else {
        model.populate(result, {path: 'creator', 
          model: 'User', 
          select: {name: 1, profile_photo_url: 1, active: 1, subtype: 1}}, 
          function(err, result){
            androidText(result, uid, function(message){
              next(err, message);
            });
          });
        }
      });
    } else {
      next({error: "no message"}, message);
    }
  }); 
};

messageSchema.statics.findSentList = function(user, oid, next){
  var model = this.model('Message');
  model.find({creator: user._id, $and: [{target: {$ne: null}}, {target: {$ne: user._id}}], group: null, organization: oid})
  .select({target: 1})
  .sort({create_date: -1})
  .exec(function(err, messages){
    if (messages) {
      model.populate(messages, {path: 'target', model: 'User', select: {_id: 1, name: 1, first_name:1, last_name: 1, profile_photo_url: 1, active: 1}}, function(err, messages) {
        var users = _.uniq(_.pluck(messages, 'target'));
        next(err, users); 
      });
    } else {
      next(err, []);
    }
  });
};

messageSchema.statics.getMessageAggregate = function(user, oid, next) {
  var userParams = {_id: 1, name: 1, first_name: 1, last_name: 1, profile_photo_url: 1, active: 1};
  var model = this.model('Message');
  var params = {
    $or: [
                {target: user._id}, 
                {creator: user._id, target: {$ne: null}}
              ], organization: oid
  };
  model.find(params)
  .select({target: 1,
    creator: 1, create_date: 1})
  .sort({create_date: -1})
  .exec(function(err, messages) {
    model.populate(messages, {path: 'target', select: userParams, model: 'User'}, function(err, messages) {

      model.populate(messages, {path: 'creator', select: userParams, model: 'User'}, function(err, messages) {
        if (messages) {
          var targets = _.pluck(messages, 'target');
          var creators = _.pluck(messages, 'creator');
          var users = _.filter(_.uniq(_.union(targets, creators), function(obj){
            return String(obj._id);
          }), function(obj){
            return String(obj._id) != String(user._id); 
          });
        }
        next(err, users);
      });
    });
  });
};

messageSchema.statics.fetchDirectMessages = function(uid, tid, oid, before, after, next){
  var userParams = {_id: 1, name: 1, first_name: 1, last_name: 1, profile_photo_url: 1, active: 1};
  var model = this.model('Message');
  var params = {$or: [{creator: uid, target: tid, organization: oid}, {creator:tid, target: uid, organization: oid}]};
  if (before) {
    params.create_date =  {$lt: before};
  }
  if (after) {
    params.create_date = {$gt: after};
  }
  model.find(params)
  .populate({path: 'creator', model: 'User', select: userParams})
  .populate({path: 'target', model: 'User', select: userParams}) 
  .sort({create_date: -1})
  .limit(25)
  .exec(function(err, messages){
    androidText(messages, uid, function(m) {
      m.reverse();
      next(err, m);
    });
  });
}

mongoose.model('Message', messageSchema);
