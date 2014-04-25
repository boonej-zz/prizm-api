/**
 * Trust Model
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _prism_home   = process.env.PRISM_HOME,
    _             = require('underscore'),
    _utils        = require(_prism_home + 'utils');

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
                          lowercase: true, required: true },
  create_date         : { type: Date, default: null },
  modify_date         : { type: Date, default: null },
  delete_date         : { type: Date, default: null },
  last_modified_by    : { type: String, default: null }
});

trustSchema.static('findTrust', function(user1, user2, callback){
  var criteria = {
    $or:[ {to: user1, from:user2},
          {to: user2, from:user1} ]
  };
  return this.findOne(criteria)
            // .select(this.selectFields('basic').join(' '))
            .exec(callback);
});

trustSchema.statics.selectFields = function(type){
  //create the short select by default
  var select = [  'from', 'to', 'status', 'type', 'create_date',
                  'modify_date', 'from_posts_count', 'from_comments_count',
                  'from_likes_count', 'to_posts_count', 'to_comments_count',
                  'to_likes_count', 'last_modified_by'];

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

trustSchema.path('status').validate(function(value){
  return /accepted|rejected|pending|cancelled/i.test(value);
});

trustSchema.pre('save', function(next){
  if(!this.create_date){
    this.create_date = new Date();
  }
  this.modify_date = new Date();
  next();
});

trustSchema.pre('update', function(next){
  this.modify_date = new Date();
  next();
});

exports.Trust = _mongoose.model('Trust', trustSchema);
