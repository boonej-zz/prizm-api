/**
 * Activity Model Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _thisapp    = require(process.cwd() + '/server.js'),
    _request    = require('request'),
    _mongoose   = require('mongoose'),
    _chai       = require('chai'),
    _expect     = _chai.expect,
    _should     = _chai.should(),
    _assert     = _chai.assert,
    _logger     = require('winston'),
    _helpers    = require(process.env.PRISM_HOME + 'test/test_helpers'),
    Post        = require(process.env.PRISM_HOME + 'models/post').Post,
    User        = require(process.env.PRISM_HOME + 'models/user').User,
    Activity    = require(process.env.PRISM_HOME + 'models/activity').Activity,
    Trust       = require(process.env.PRISM_HOME + 'models/user').Trust;

describe('Activity Model Unit Tests', function(done){
  var test_user;
  var test_target_user;
  var test_activity;
  var test_activity_failed;
  var test_activity_success;
  var test_activity_success_date;
  var test_token;
  var test_code;
  var test_client;

  var setupUsers = function(cb){
    var u = new User({
      first_name: 'the',
      last_name: 'godfather',
      email: 'tgf'+ Math.floor((Math.random()*100)+1)+'@italy.com',
      password: 'horsesHead'
    }).save(function(err, user){
      if(err) throw err;
      test_user = user;
      var u2 = new User({
        first_name: '2cool',
        last_name: 'chris',
        email: '2cool'+Math.floor((Math.random()*100)+1)+'@thenineties.com',
        password: 'oldschool'
      }).save(function(err, user2){
        if(err) throw err;
        test_target_user = user2;
        cb();
      });
    });
  };

  var executeFailedActivity = function(cb){
    new Activity({}).save(function(err){
      test_activity_failed = err;
      cb();
    });
  };

  var executeSuccessActivity = function(cb){
    new Activity({
      type: 'follow',
      user: test_user._id,
      target: test_target_user._id
    }).save(function(err, result){
      test_activity_success_date = Date.now();
      test_activity_success = result;
      cb();
    });
  };

    var executeRequest = function(method, url, body, cb){
    _request({
      method: method,
      strictSSL: false,
      json: true,
      url: 'https://localhost:3000/' + url,
      headers: {"Authorization":"Bearer " + test_token.access_token},
      body: (!body)? {} : body
    }, function(err,result){
      if(cb) cb(err, result.body);
    });
  };

  before(function(done){
    _helpers.createTestToken(function(token, code, client){
      test_token = token;
      test_code = code;
      test_client = client;
      setupUsers(function(){
        executeFailedActivity(function(){
          executeSuccessActivity(function(){
            done();
          });
        });
      });
    });
  });

  after(function(done){
    Activity.remove({}, function(){
      done();
    });
  });

  describe('Creating an Activity', function(done){
    it('should create an activity successfully', function(done){
      _expect(test_activity_success).to.have.property('_id');
      _expect(test_activity_success._id.toString().length).to.be.above(0);
      done();
    });
    it('should set the create_date on creation', function(done){
      var date = test_activity_success_date.valueOf();
      _expect(test_activity_success).to.have.property('create_date');
      _expect(test_activity_success).to.not.equal(null);
      _expect(test_activity_success).to.not.equal(false);
      _expect(test_activity_success).to.not.equal('undefined');
      _expect(test_activity_success.create_date.valueOf()).to.be.within(date-5, date+5);
      done();
    });
    it('should require an activity type', function(done){
      var err = test_activity_failed;
      _expect(err.message).to.equal('Validation failed');
      _expect(err.errors).to.have.property('type');
      _expect(err.errors.type.type).to.equal('required');
      done();
    });
    it('should require an activity user', function(done){
      var err = test_activity_failed;
      _expect(err.message).to.equal('Validation failed');
      _expect(err.errors).to.have.property('user');
      _expect(err.errors.user.type).to.equal('required');
      done();
    });
    it('should require an activity target user', function(done){
      var err = test_activity_failed;
      _expect(err.message).to.equal('Validation failed');
      _expect(err.errors).to.have.property('target');
      _expect(err.errors.target.type).to.equal('required');
      done();
    });
  });
  describe('Testing a User Activity', function(done){
    var usr_activity_post;
    var usr_activity_comment;
    var usr_activity_post_like;
    var usr_activity_comment_like;
    var usr_activity_trust;

    it('should track when a user creates a post', function(done){
       var body = {
        text: 'activity post test check',
        creator: test_user._id,
        category: 'experiences',
        target_id: test_target_user._id
      };
      executeRequest('POST','users/'+test_target_user._id+'/posts',body,function(err, result){
        usr_activity_post = result.data[0];
        Activity.findOne({target:test_target_user._id, type: 'post'},function(err, result){
          if(err) throw err;
          _expect(result.object.creator.toString()).to.equal(test_user._id.toString());
          _expect(result.type).to.equal('post');
          _expect(result.object.text).to.equal(body.text);
          _expect(result.user.toString()).to.equal(test_user._id.toString());
          done();
        });
      });
    });
    it('should track when a user makes a comment', function(done){
      var body = {
        text: 'activity comment test check',
        creator: test_user._id
      };
      executeRequest('POST', 'posts/'+usr_activity_post._id+'/comments', body, function(e, c){
        usr_activity_comment = c.data[0];
        Activity.findOne({type: 'comment' }, function(error, result){
          _expect(result.object.creator._id.toString()).to.equal(test_user._id.toString());
          _expect(result.type).to.equal('comment');
          _expect(result.object.text).to.equal(body.text);
          done();
        });
      });
    });
    it('should track when a user likes a post', function(done){
      executeRequest('POST', 'posts/'+usr_activity_post._id+'/like',
                     {creator: test_user._id},
                     function(err, like){
        usr_activity_post_like = like.data[0];
        Activity.findOne({type:'like', context: 'post'}, function(err, result){
          _expect(result.type).to.equal('like');
          _expect(result.context).to.equal('post');
          _expect(result.user.toString()).to.equal(test_user._id.toString());
          done();
        });
      });
    });
    it('should track when a user likes a comment', function(done){
      var url = 'posts/'+usr_activity_post._id+'/comments/'+usr_activity_comment.comments._id+'/like';
      executeRequest('POST', url, {creator: test_user._id},function(err, like){
        Activity.findOne({type:'like', context: 'comment'}, function(err, result){
          _expect(result.type).to.equal('like');
          _expect(result.context).to.equal('comment');
          _expect(result.target.toString()).to.equal(usr_activity_comment.comments._id.toString());
          _expect(result.user.toString()).to.equal(test_user._id.toString());
          _expect(result.object.likes._id.toString()).to.equal(test_user._id.toString());
          done();
        });
      });
    });
    it('should track when a user unlikes a post', function(done){
      var url = 'posts/'+usr_activity_post._id+'/unlike';
      executeRequest('POST', url, {creator: test_user._id}, function(err, unlike){
        if(err) throw err;
        if(unlike.error) throw unlike.error;
        Activity.findOne({type: 'unlike', context: 'post'}, function(err, result){
          _expect(result.type).to.equal('unlike');
          _expect(result.context).to.equal('post');
          _expect(result.target.toString()).to.equal(usr_activity_post._id.toString());
          _expect(result.user.toString()).to.equal(test_user._id.toString());
          done();
        });
      });
    });
    it('should track when a user unlikes a comment', function(done){
      var url = 'posts/'+usr_activity_post._id+'/comments/'+usr_activity_comment.comments._id+'/unlike';
      executeRequest('POST', url, {creator: test_user._id}, function(err, unlike){
        if(err) throw err;
        if(unlike.error) throw unlike.error;
        Activity.findOne({type: 'unlike', context: 'comment'}, function(err, result){
          _expect(result.type).to.equal('unlike');
          _expect(result.context).to.equal('comment');
          _expect(result.user.toString()).to.equal(test_user._id.toString());
          _expect(result.target.toString()).to.equal(usr_activity_comment.comments._id.toString());
          done();
        });
      });
    });
    it('should track when a user requests a trust', function(done){
      var url = 'users/'+test_target_user._id+'/trusts';
      executeRequest('POST', url, {creator: test_user._id}, function(err, trust){
        if(err) throw err;
        if(trust.error) throw trust.error;
        usr_activity_trust = trust.data[0];
        Activity.findOne({type: 'trust', action: 'request', context: null}, function(err, result){
          _expect(result.type).to.equal('trust');
          _expect(result.context).to.equal(null);
          _expect(result.action).to.equal('request');
          _expect(result.user.toString()).to.equal(test_user._id.toString());
          _expect(result.target.toString()).to.equal(test_target_user._id.toString());
          done();
        });
      });
    });
    it('should track when a user cancels a trust', function(done){
      var url = 'users/'+test_target_user._id+'/trusts/'+usr_activity_trust._id;
      executeRequest('PUT', url, {status: 'cancelled'}, function(err, status){
        if(err) throw err;
        if(status.error) throw status.error;
        Activity.findOne({type: 'trust', action: 'cancelled'}, function(err, result){
          _expect(result.type).to.equal('trust');
          _expect(result.action).to.equal('cancelled');
          _expect(result.target.toString()).to.equal(test_target_user._id.toString());
          done();
        });
      });
    });
    it('should tracl when a user recreates a trust request', function(done){
      var url = 'users/'+test_target_user._id+'/trusts';
      executeRequest('POST', url, {creator: test_user._id}, function(err, trust){
        if(err) throw err;
        if(trust.error) throw trust.error;
        usr_activity_trust = trust.data[0];
        Activity.findOne({type: 'trust', action: 'request', context: 'was_cancelled'}, function(err, result){
          _expect(result.type).to.equal('trust');
          _expect(result.action).to.equal('request');
          _expect(result.context).to.equal('was_cancelled');
          done();
        });
      });
    });
    it('should track when a user accpets a trust request', function(done){
      var url = 'users/'+test_target_user._id+'/trusts/'+usr_activity_trust._id;
      executeRequest('PUT', url, {status: 'accepted'}, function(err, status){
        if(err) throw err;
        if(status.error) throw status.error;
        Activity.findOne({type: 'trust', action: 'accepted'}, function(err, result){
          _expect(result.type).to.equal('trust');
          _expect(result.action).to.equal('accepted');
          _expect(result.user.toString()).to.equal(test_user._id.toString());
          _expect(result.target.toString()).to.equal(test_target_user._id.toString());
          done();
        });
      });
    });
    it('should track when a user is followed', function(done){
      var url = 'users/'+test_target_user._id+'/follow';
      executeRequest('POST', url, {creator: test_user._id}, function(err, follow){
        if(err) throw err;
        if(follow.error) throw follow.error;
        Activity.findOne({type: 'follow'}, function(err, result){
          _expect(result.type).to.equal('follow');
          _expect(result.user.toString()).to.equal(test_user._id.toString());
          _expect(result.target.toString()).to.equal(test_target_user._id.toString());
          done();
        });
      });
    });
    it('should track when a user is unfollowed', function(done){
      var url = 'users/'+test_target_user._id+'/unfollow';
      executeRequest('POST', url, {creator: test_user._id}, function(err, follow){
        if(err) throw err;
        if(follow.error) throw follow.error;
        Activity.findOne({type: 'unfollow'}, function(err, result){
          _expect(result.type).to.equal('unfollow');
          _expect(result.user.toString()).to.equal(test_user._id.toString());
          _expect(result.target.toString()).to.equal(test_target_user._id.toString());
          done();
        });
      });
    });
  });
});
