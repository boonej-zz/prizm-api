/**
 * ActivityListener Class
 *
 * @author  DJ Hayden <dj.hayden@stablekernel.com>
 */

/**
 * Module dependencies.
 */
var EventEmitter  = require('events').EventEmitter,
    _util         = require('util'),
    _mongoose     = require('mongoose'),
    _logger       = require(process.env.PRISM_HOME + 'logs.js'),
    Post          = require(process.env.PRISM_HOME + 'models/post').Post,
    User          = require(process.env.PRISM_HOME + 'models/user').User,
    Comment       = require(process.env.PRISM_HOME + 'models/post').Comment,
    Trust         = require(process.env.PRISM_HOME + 'models/user').Trust,
    Activity      = require(process.env.PRISM_HOME + 'models/activity').Activity;

/**
 * Expose `ActivityListener`.
 */
module.exports = ActivityListener;

/**
 * Initializes The `ActivityListener`
 *
 * @param {[type]} action [description]
 */
function ActivityListener(options){
  EventEmitter.call(this);
  var self = this;
  this.options = (!options) ? {} : options;

  process.on('activity', function(object){
    if(typeof object.type !== 'undefined'){
      _logger.log('info', 'activity event emitted', object);
      self.activityHandler(object);

    }else{
      _logger.log('error', 'activity events object does not have a `type` property', object);
    }
  });
}

/**
 * Inherit from `EventEmitter.prototype`
 */
_util.inherits(ActivityListener, EventEmitter);

/**
 * Checks if NODE_ENV is set to test
 *
 * @return {Boolean}
 */
var isInTestMode = function(){
  if(process.env.NODE_ENV === 'test'){
    return true;
  }else{
    return false;
  }
};

/**
 * [activityHandler description]
 * @param  {[type]} object [description]
 * @return {[type]}        [description]
 */
ActivityListener.prototype.activityHandler = function(object){
  switch(object.type){
    case 'follow':
      this.postActivity(object);
      break;
    case 'unfollow':
      this.postActivity(object);
      break;
    case 'like':
      this.postActivity(object);
      break;
    case 'unlike':
      this.postActivity(object);
      break;
    case 'post':
      this.postActivity(object);
      break;
    case 'repost':
      this.postActivity(object);
      break;
    case 'comment':
      this.postActivity(object);
      break;
    case 'trust':
      this.postActivity(object);
      break;
    case 'user':
      //user
      break;
    default:
      //something for the system to track activity?
      break;
  }
};

/**
 * Post Activity Handler - Creates an activity record with a post type
 *
 * @param  {Object} post The object emitted from the post route
 * @return {[type]}      [description]
 */
ActivityListener.prototype.postActivity = function(post){
  if(post.user && post.target){
    new Activity({
      type: post.type,
      user: post.user,
      target: post.target,
      context: (post.context) ? post.context : null,
      scope: (post.scope) ? post.scope : null,
      object: (post.object) ? post.object : null,
      action: (post.action) ? post.action : null
    }).save(function(err, activity_saved){
      if(err){
        _logger.log('error', 'Error trying to save '+ post.type +' activity', post);
        if(isInTestMode) process.emit('activity_'+ post.type +'_error', err);
      } else{
        _logger.log('info', 'Successfully Logged '+ post.type +' Activity', activity_saved);
        if(isInTestMode) process.emit('activity_'+ post.type +'_success', activity_saved);
      }
    });
  }else{
    _logger.log('error',
                'Validation Error trying to create ' + post.type +
                'activity object.',
                { error: 'Activity.user && Activity.target need to be set',
                  object: post });
  }
};

ActivityListener.prototype.commentActivity = function(comment){
  if(comment.user && comment.target){
    new Activity({
      type: comment.type,
      user: comment.user,
      target: comment.target,
      scope: (comment.scope) ? comment.scope : null,
      object: (comment.object) ? comment.object : null
    }).save(function(err, activity_saved){
      if(err){
        _logger.log('error', 'Error trying to save ' + comment.type + ' activity', comment);
      }else{
        _logger.log('info', 'Successfully Logged ' + comment.type + ' Activity', activity_saved);
      }
    });
  }else{
    _logger.log('error',
                'Validation Error trying to create comment activity object',
                {error: 'Activity.user && Activity.target'});
  }
};


