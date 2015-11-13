/**
 * Trust Model
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _prism_home   = process.env.PRISM_HOME,
    _             = require('underscore'),
    _utils        = require(_prism_home + 'utils'),
    User          = require(_prism_home + 'models/user').User;
var time       = require('../lib/helpers/date_time');

var trustSchema = new _mongoose.Schema({
  from                : { type: _mongoose.Schema.Types.ObjectId,
                          ref: 'User',
                          required: true },
  from_posts          : { type: Array, default: [] },
  from_comments       : { type: Array, default: [] },
  from_post_likes     : { type: Array, default: [] },
  from_comment_likes  : { type: Array, default: [] },
  from_posts_count    : { type: Number, default: 0 },
  from_comments_count : { type: Number, default: 0 },
  from_likes_count    : { type: Number, default: 0 },
  to                  : { type: _mongoose.Schema.Types.ObjectId,
                          ref: 'User',
                          required: true },
  to_posts            : { type: Array, default: [] },
  to_comments         : { type: Array, default: [] },
  to_post_likes       : { type: Array, default: [] },
  to_comment_likes    : { type: Array, default: [] },
  to_posts_count      : { type: Number, default: 0 },
  to_comments_count   : { type: Number, default: 0 },
  to_likes_count      : { type: Number, default: 0 },
  type                : { type: String, default: null },
  status              : { type: String, default: 'pending',
                          lowercase: true },
  create_date         : { type: Date, default: null },
  modify_date         : { type: Date, default: null },
  delete_date         : { type: Date, default: null },
  last_modified_by    : { type: String, default: null },
  from_score          : { type: Number, default: null },
  to_score            : { type: Number, default: null }
});

var activityFields = {
  _id: 1,
  from: 1,
  to: 1,
  modify_date: 1,
  create_date: 1,
  status: 1
};

var userFields = {
  _id: 1,
  name: 1,
  profile_photo_url: 1,
  type: 1,
  subtype: 1
}

trustSchema.static('findTrust', function(user1, user2, callback){
  var criteria = {
    $or:[ {to: user1, from:user2},
          {to: user2, from:user1} ]
  };
  return this.findOne(criteria)
            // .select(this.selectFields('basic').join(' '))
            .exec(callback);
});

trustSchema.statics.fetchUserTrustCount = function(user_id, callback){
  var criteria = {$or: [{to: user_id}, {from: user_id}]};
  return this.count(criteria).exec(callback);
};

trustSchema.statics.selectFields = function(type){
  //create the short select by default
  var select = [  'from', 'to', 'status', 'type', 'create_date',
                  'modify_date', 'from_posts_count', 'from_comments_count',
                  'from_likes_count', 'to_posts_count', 'to_comments_count',
                  'to_likes_count', 'last_modified_by', 'from_score', 'to_score'];

  if(type && type === 'basic'){
    _.union(select, [
      'from_posts', 'from_comments', 'from_post_likes',
      'to_posts', 'to_comments', 'to_post_likes',
      'to_comment_likes', 'from_comment_likes'
    ]);
  }

  return select;
};

trustSchema.statics.canResolve = function(){
  return [
    {from: {identifier: '_id', model: 'User'}},
    {to: {identifier: '_id', model: 'User'}},
    {from_posts: {identifier: '_id', model: 'Post'}},
    {from_comments: {identifier: '_id', model: 'Post'}},
    {from_post_likes: {identifier: '_id', model: 'Post'}},
    {from_comment_likes: {identifier: '_id', model: 'Post'}},
    {to_posts: {identifier: '_id', model: 'Post'}},
    {to_comments: {identifier: '_id', model: 'Post'}},
    {to_post_likes: {identifier: '_id', model: 'Post'}},
    {to_comment_likes: {identifier: '_id', model: 'Post'}}
  ];
};

trustSchema.methods.updateUsersTrustCount = function(callback){
  var self = this;
  var to_user_id = this.to;
  var from_user_id = this.from;
  _mongoose.model('User').updateTrustCount(to_user_id, function(err, success){
    if(err){
      callback(err);
      return;
    }
    _mongoose.model('User').updateTrustCount(from_user_id, function(err, success){
      if(err){
        callback(err);
        return;
      }
      callback(null);
    });
  });
};

trustSchema.methods.calculateTrustScore = function(){
  var from_score = 0, to_score = 0;
  if(this.from_posts_count) from_score = from_score + (this.from_posts_count * 0.45);
  if(this.from_comments_count) from_score = from_score + (this.from_comments_count * 0.35);
  if(this.from_likes_count) from_score = from_score + (this.from_likes_count * 0.20);
  if(this.to_posts_count) to_score = to_score + (this.to_posts_count * 0.45);
  if(this.to_comments_count) to_score = to_score + (this.to_comments_count * 0.35);
  if(this.to_likes_count) to_score = to_score + (this.to_likes_count * 0.20);

  this.to_score = to_score;
  this.from_score = from_score;

};

trustSchema.pre('save', function(next){
  if(!this.create_date){
    this.create_date = new Date();
  }
  this.modify_date = new Date();
  this.calculateTrustScore();
  next();
});

trustSchema.pre('update', function(next){
  this.modify_date = new Date();
  this.calculateTrustScore();
  next();
});

trustSchema.statics.fetchTrustActivityForUser = function(uid, last, next){
  var model = this.model('Trust');
  var criteria = {
    to: uid, status: 'pending'
  };
  if (last) {
    criteria.modify_date = {$lt: last};
  }
  model.find(criteria)
  .select(activityFields)
  .sort({modify_date: -1})
  .limit(15)
  .populate({path: 'from', model: 'User', select: userFields})
  .exec(function(err, trusts){
    next(err, flattenActivity(trusts));
  });
};

var flattenActivity = function(trusts) {
  if (!_.isArray(trusts)) {
    return null;
  }
  var result = [];
  _.each(trusts, function(t){
    t = t.toObject();
    t.from_name = t.from.name;
    t.from_id = t.from._id;
    t.from_profile_photo_url = t.from.profile_photo_url;
    t.from_type = t.from.type;
    t.from_subtype = t.from.subtype;
    delete t.from;
    t.time_since = time.timeSinceFormatter(t.modify_date); 
    result.push(t);
  });
  return result;
};

exports.Trust = _mongoose.model('Trust', trustSchema);
