/**
 * Posts Route Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _request      = require('request'),
    _chai         = require('chai'),
    _expect       = _chai.expect,
    _should       = _chai.should(),
    _assert       = _chai.assert,
    _thisapp      = require(process.cwd() + '/server'),
    _prism_home   = process.env.PRISM_HOME,
    _auth_model   = require(_prism_home + 'models/auth'),
    _follow       = require(_prism_home + 'routes/follow'),
    _t_helpers    = require(_prism_home + 'test/test_helpers'),
    User          = require(_prism_home + 'models/user').User,
    Trust         = require(_prism_home + 'models/trust').Trust,
    Post          = require(_prism_home + 'models/post').Post;

describe('Posts Route Unit Tests', function(done){
  var mark, edwardo, cameron, erica, sean, maryolin, DJ;
  var follower = null;
  var followee = null;
  var follow_result = { followee: null, follower: null};
  var request_result = null;
  var test_user = null;
  var test_token = null;
  var test_client = null;
  var test_code = null;
  var test_post1 = null;
  var test_post2 = null;
  var test_post3 = null;
  var test_post4 = null;
  var test_post5 = null;
  var test_post6 = null;
  var test_post7 = null;
  var test_post8 = null;
  var test_comment_id = null;
  var test_comment_id2 = null;

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

  var setupSocialNetworkUsers = function(cb){
    var users = _t_helpers.fetchFakeUsersArray();
    User.create(users, function(err, m, e, c, e2, s, m2){
      if(err) throw err;
      mark = m;
      edwardo = e;
      cameron = c;
      erica = e2;
      sean = s;
      maryolin = m2;
      cb();
    });
  };

  var executeFollowRequest = function(u_follower, u_followee, cb){
    _request({
      method: 'POST',
      strictSSL: false,
      json: true,
      url: 'https://localhost:3000/users/'+u_followee._id+'/follow',
      headers: {"Authorization": 'Bearer ' + test_token.access_token},
      body: {creator: u_follower._id}
    }, function(err, result){
      if(cb) cb(err, result.body);
    });
  };

  var executeLikeRequest = function(type, u_creator, post_id, cb){
    _request({
      method: 'POST',
      strictSSL: false,
      json: true,
      url: 'https://localhost:3000/posts/'+post_id+'/'+type,
      headers: {"Authorization" : 'Bearer ' + test_token.access_token},
      body: {creator: u_creator}
    }, function(err, res){
      if(cb) cb(err, res.body);
    });
  };

  var executeCommentLikeRequest = function(type, u_creator, post_id, comment_id, cb){
    _request({
      method: "POST",
      strictSSL: false,
      json:true,
      url: 'https://localhost:3000/posts/'+post_id+'/comments/'+comment_id+'/'+type,
      headers: {'Authorization':'Bearer ' + test_token.access_token},
      body: {creator: u_creator}
    }, function(err, response){
      if(cb) cb(err, response.body);
    });
  };

  var executeAddCommentRequest = function(u_creator, post_id, cb){
    _request({
      method: 'POST',
      strictSSL: false,
      json: true,
      url: 'https://localhost:3000/posts/' + post_id + '/comments',
      headers: {"Authorization" : "Bearer " + test_token.access_token},
      body: {creator: u_creator, text: 'test commenting on this post'}
    }, function(err, result){
      if(cb) cb(err, result.body);
    });
  };

  var executeDeleteCommentRequest = function(post_id, comment_id, cb){
    _request({
      method: 'DELETE',
      strictSSL: false,
      json: true,
      url: 'https://localhost:3000/posts/'+post_id+'/comments/'+comment_id,
      headers: {'Authorization' : 'Bearer '+ test_token.access_token}
    }, function(err, result){
      if(cb) cb(err, result.body);
    });
  };

  var executeDeletePostRequest = function(post_id, creator, cb){
    executeRequest('DELETE', 'posts/'+post_id, {creator: creator}, function(err, res){
      if(cb) cb(err, res);
    });
  };

  var executeFlagPostRequest = function(post_id, reporter_id, cb){
    executeRequest('POST', 'posts/'+post_id+'/flag', {reporter: reporter_id}, function(err, res){
      if(cb) cb(err, res);
    });
  };

  var executeUpdatePostRequest = function(post_id, updated_post, cb){
    executeRequest('PUT', 'posts/'+post_id, updated_post, function(err, res){
      if(cb) cb(err,res);
    });
  };

  before(function(done){
    _t_helpers.createTestUser(function(user){
      test_user = user;
      _t_helpers.createTestToken(function(token, code, client){
        test_token = token;
        test_code = code;
        test_client = client;
        _t_helpers.fetchFixtureTestUsers(function(users){
          mark = users.mark;
          edwardo = users.edwardo;
          cameron = users.cameron;
          erica = users.erica;
          sean = users.sean;
          maryolin = users.maryolin;
          DJ = users.DJ;
          _t_helpers.fetchFixturePosts(function(posts){
            test_post1 = posts[0];
            test_post2 = posts[1];
            test_post3 = posts[2];
            test_post4 = posts[3];
            test_post5 = posts[4];
            test_post6 = posts[5];
            test_post7 = posts[6];
            test_post8 = posts[7];
            done();
          });
        });
      });
    });
  });

  //TODO: need to actually update
  describe('Testing Fetching a Post by Id', function(done){
    it('should return the entire post object', function(done){
      done();
    });
  });
  //TODO: update comment to a post test
  describe.skip('Testing Adding a Comment to a Post', function(done){
    it('should add a comment to a specified post', function(done){
      executeAddCommentRequest(test_user._id, test_post1._id, function(err, body){
        _expect(body.metadata.success).to.equal(true);
        _expect(body.data[0]).to.have.property('comments');
        _expect(body.data[0]).to.have.property('comments_count');
        _expect(body.data[0].comments).to.have.property('_id');
        _expect(body.data[0].comments).to.have.property('creator');
        _expect(body.data[0].comments).to.have.property('text');
        _expect(body.data[0].comments).to.have.property('create_date');
        _expect(body.data[0].comments_count).to.equal(1);
        _expect(body.data[0].comments.creator._id).to.equal(test_user._id.toString());
        _expect(body.data[0].comments.text).to.equal('test commenting on this post');
        //set comments id for to use for further test cases
        test_comment_id = body.data[0].comments._id;
        done();
      });
    });
    it('should fetch comments from a specific post', function(done){
      executeAddCommentRequest(edwardo._id, test_post1._id, function(err, body){
        if(err) throw err;
        _request({
          method: 'GET',
          json: true,
          strictSSL: false,
          headers: {"Authorization" : "Bearer " + test_token.access_token},
          url: 'https://localhost:3000/posts/'+test_post1._id+'/comments'
        }, function(err, result){
          done();
        });
      });
    });
  });
  //TODO: update test
  describe.skip('Testing Adding Hash Tags to a User Post', function(done){
    it('should set an array of hash tags to a users post', function(done){
      _request({
        method: 'POST',
        url: 'https://localhost:3000/users/'+test_user._id+'/posts',
        strictSSL: false,
        json:true,
        headers: {"Authorization" : "Bearer "+ test_token.access_token},
        body: {
          creator: mark._id,
          category: 'personal',
          text: 'check this post out',
          hash_tags: ['testing', 'coolio', 'dangerousMinds']
        }
      }, function(err, result){
        _expect(result.body.metadata.success).to.equal(true);
        _expect(result.body.data[0]).to.have.property('hash_tags');
        _expect(result.body.data[0].hash_tags.length).to.be.above(0);
        done();
      });
    });
    it('should retrieve the array of hash_tags from a post obejct', function(done){
      _request({
        method: 'GET',
        url: 'https://localhost:3000/users/'+test_user._id+'/posts',
        strictSSL:false ,
        json:true,
        headers: {"Authorization" : "Bearer " + test_token.access_token}
      }, function(err, result){
        _expect(result.body.metadata.success).to.equal(true);
        var data = result.body.data;
        for(var i=0; i < data.length; i++){
          _expect(data[i]).to.have.property('hash_tags');
          if(data[i].hash_tags_count > 0){
            var hashtags = data[i].hash_tags;
            _expect(hashtags[0]).to.equal('testing');
            _expect(hashtags[1]).to.equal('coolio');
            _expect(hashtags[2]).to.equal('dangerousMinds');
          }
        }
        done();
      });
    });
  });
  //TODO: update test
  describe.skip('Testing Fetching A Specific Comment', function(done){
    it('should fetch a specific comment and return the comment ' +
      ' object with populated creator', function(done){
        _request({
          method: 'GET',
          url: 'https://localhost:3000/posts/'+test_post1._id+
                '/comments/'+test_comment_id,
          strictSSL:false,
          json:true,
          headers: {'Authorization' : 'Bearer ' + test_token.access_token}
        }, function(err, result){
          _expect(result.body.metadata.success).to.equal(true);
          _expect(result.body.data[0]).to.have.property('_id');
          _expect(result.body.data[0]).to.have.property('creator');
          _expect(result.body.data[0]).to.have.property('likes_count');
          _expect(result.body.data[0]).to.have.property('likes');
          _expect(result.body.data[0]).to.have.property('create_date');
          _expect(result.body.data[0]).to.have.property('text');
          _expect(result.body.data[0].creator).to.have.property('_id');
          _expect(result.body.data[0].creator).to.have.property('name');
          _expect(result.body.data[0].creator).to.have.property('first_name');
          _expect(result.body.data[0].creator).to.have.property('last_name');
          _expect(result.body.data[0].creator).to.have.property('profile_photo_url');
          _expect(result.body.data[0]._id).to.equal(test_comment_id.toString());
          done();
        });
      });
  });
  //TODO: update test
  describe.skip('Testing Removing A Specific Comment', function(done){
    it('should remove the comment from the posts comment array', function(done){
      executeAddCommentRequest(cameron._id, test_post1._id, function(err, body){
        if(err) throw err;
        var delete_id = body.data[0].comments._id;
        executeDeleteCommentRequest(test_post1._id, delete_id, function(error, delete_body){
          _expect(delete_body.metadata.success).to.equal(true);
          _expect(delete_body.data[0]).to.have.property('comments_count');
          _expect(delete_body.data[0]).to.have.property('comments');
          _expect(delete_body.data[0].comments_count).to.equal(2);
          _expect(delete_body.data[0].comments.length).to.equal(0);
          done();
        });
      });
    });
  });
  //TODO: update test
  describe.skip('Testing Like A Specific Post Comment', function(done){
    it('should like the comment adding creator to comments.like array', function(done){
      executeAddCommentRequest(sean._id, test_post1._id);
      executeAddCommentRequest(erica._id, test_post1._id, function(err, erica_comment){
        test_comment_id2 = erica_comment.data[0].comments._id;
      executeCommentLikeRequest('like', mark._id, test_post1._id, test_comment_id, function(err, result){
        if(err) throw err;
        _expect(result.metadata.success).to.equal(true);
        _expect(result.data[0]).to.have.property('likes');
        _expect(result.data[0]).to.have.property('likes_count');
        _expect(result.data[0].likes._id).to.equal(mark._id.toString());
        _expect(result.data[0].likes_count).to.be.above(0);
        executeCommentLikeRequest('like', sean._id, test_post1._id, test_comment_id2, function(err, test_result){
          done();
        });
        });
      });
    });
  });
  //TODO: update test
  describe.skip('Testing Unliking A Specific Post Comment', function(done){
    it('should unlike the comment removing the creator _id from the comments.like array', function(done){
      executeCommentLikeRequest('like', edwardo._id, test_post1._id, test_comment_id, function(error, second_comment){
        if(error) throw error;
        _expect(second_comment.metadata.success).to.equal(true);
        _expect(second_comment.data[0].likes_count).to.equal(2);

        executeCommentLikeRequest('unlike', edwardo._id, test_post1._id, test_comment_id, function(err, result){
          if(err) throw err;
          _expect(result.metadata.success).to.equal(true);
          _expect(result.data[0]).to.have.property('likes');
          _expect(result.data[0]).to.have.property('likes_count');
          _expect(result.data[0].likes.length).to.equal(0);
          _expect(result.data[0].likes_count).to.equal(1);
          done();
        });
      });
    });
  });

  describe('Testing A Users News Feed', function(done){
    var test_user_post;
    var feed_result;
    var feed_error;
    var trust;

    //create a user post to ensure it shows up in the news feed
    var createTestUserPost = function(cb){
      new Post({
        text: 'test user post',
        category: 'aspirations',
        creator: test_user._id,
        target_id: test_user._id
      }).save(function(err, result){
        if(err) throw err;
        test_user_post = result;
        cb();
      });
    };

    //setup test_user to follow mark
    before(function(done){
      //make the assumption everything is fine
      createTestUserPost(function(){
        _request({
          method: 'GET',
          url: 'https://localhost:3000/users/' + test_user._id+'/feed',
          json: true,
          strictSSL: false,
          headers: {"Authorization":"Bearer "+test_token.access_token}
        },function(err, result){
          feed_error = err;
          feed_result = result.body;
          //create trust with a user
          new Trust({
            to: test_user._id, 
            from: sean._id, 
            status: 'accepted'
          }).save(function(er, res){
            if (er) throw er;
            trust = res;
            done();
          });
        });
      });
    });
    it('should fetch a users news feed', function(done){
      debugger;
      _expect(feed_result.metadata.success).to.equal(true);
      _expect(feed_result.data.length).to.be.above(3);
      done();
    });
    it('should include the users posts', function(done){
      var users_post_in_results = false;
      for(var i = 0; i < feed_result.data.length; i++){
        if(feed_result.data[i]._id.toString() === test_user_post._id.toString()){
          users_post_in_results = true;
        }
      }
      _assert.isTrue(users_post_in_results, 'Users Post _id should be returned home feed');
      done();
    });
  });
  describe('Testing Fetching A Post Like by Post Id && Requestor Id', function(done){
    it('should return the post object with likes & count', function(done){
      executeLikeRequest('like', edwardo._id, test_post3._id, function(err, res){
        _request({
          method: 'GET',
          strictSSL: false,
          json: true,
          headers: {"Authorization" : "Bearer "+ test_token.access_token},
          url: 'https://localhost:3000/posts/'+test_post3._id+'/like/'+edwardo._id
        }, function(err, result){
          _expect(result.body.metadata.success).to.equal(true);
          _expect(result.body.data[0].likes_count).to.be.above(0);
          _expect(result.body.data[0].likes[0]._id).to.equal(edwardo._id.toString());
          done();
        });
      });
    });
  });
  describe('Testing Like a Post', function(done){
    it('should update the posts record after a successful like', function(done){
      executeLikeRequest('like',mark._id, test_post1._id, function(err, res){
        _expect(res.metadata.success).to.equal(true);
        _expect(res.data[0].likes_count).to.equal(1);
        _expect(res.data[0].likes[0]._id).to.equal(mark._id.toString());
        done();
      });
    });
    it('should return an error with invalid post_id', function(done){
      done();
    });
    it('should return an error with no creator identifier in post body', function(done){
      executeLikeRequest('like', null, test_post1._id, function(err, res){
        _assert.ok(res.error, 'Result Error does not exist');
        _expect(res.error.error).to.equal('invalid_request');
        _assert.lengthOf(res.data, 0, 'Result data should be empty');
        done();
      });
    });
  });
  describe('Testing Unlike a Post', function(done){
    beforeEach(function(done){
      executeLikeRequest('like', test_user._id, test_post2._id, function(err, res){
        if(err) throw err;
        done();
      });
    });

    it('should remove the like from the posts record on successful unlike', function(done){
      executeLikeRequest('unlike', test_user._id, test_post2._id, function(err, res){
        _expect(res.metadata.success).to.equal(true);
        _expect(res.data[0].likes_count).to.equal(0);
        _assert.lengthOf(res.data[0].likes, 0, 'Likes array should be empty');
        done();
      });
    });
  });
  describe('Testing Deleting a POST', function(done){
    var delete_error, delete_result, post_to_delete, post_to_delete_updated;

    before(function(done){
      post_to_delete = test_post4;
      executeDeletePostRequest(post_to_delete._id, mark._id, function(err, result){
        delete_error = err;
        delete_result = result;
        done();
      });
    });

    it('should succesfully "remove" a specified post & return message', function(done){
      _expect(delete_result.metadata.success).to.equal(true);
      _expect(delete_result.data[0]).to.have.property('message');
      _expect(delete_result.data[0].message).to.equal('Post Successfully Removed');
      done();
    });
    it('should update the status of that document to deleted', function(done){
      Post.findOne({_id: post_to_delete}, function(err, post){
        post_to_delete_updated = post;
        _expect(post.status).to.equal('deleted');
        done();
      });
    });
    it('should update the delete_date when deleted', function(done){
      var tdate = Date.now();
      post_to_delete_updated.delete_date.valueOf()
      .should.be.within(tdate.valueOf() -10, tdate.valueOf() + 10);
      done();
    });
    it('should return false with error when not passed a creator in body', function(done){
      executeDeletePostRequest(post_to_delete._id, null, function(err, result){
        _expect(result.metadata.success).to.equal(false);
        _expect(result.data.length).to.equal(0);
        _expect(result.error.error).to.equal('invalid_request');
        done();
      });
    });
    it('should return false with an error when passed an invalid post_id', function(done){
      executeDeletePostRequest('123123jhjsdf', mark._id, function(err, result){
        _expect(result.metadata.success).to.equal(false);
        _expect(result.data.length).to.equal(0);
        _expect(result.error.error).to.equal('unable_to_delete_post');
        done();
      });
    });
    it('should return false with error when sent a creator id that didnt create the post', function(done){
      executeDeletePostRequest(post_to_delete._id, sean._id, function(err, result){
        _expect(result.metadata.success).to.equal(false);
        _expect(result.data.length).to.equal(0);
        _expect(result.error.error).to.equal('unable_to_delete_post');
        done();
      });
    });
  });
  describe('Testing Flagging a Post `Inappropriate`', function(done){
    var post_to_flag, fresult, ferror, fpost;
    before(function(done){
      post_to_flag = test_post5;
      executeFlagPostRequest(post_to_flag._id, maryolin._id, function(err, result){
        ferror = err;
        fresult = result;
        Post.findOne({_id: post_to_flag._id}, function(error, post){
          if(error) throw error;
          fpost = post;
          done();
        });
      });
    });
    it('should successfully return true with valid post_id in message', function(done){
      var message_to_validate = 'Post ' + post_to_flag._id.toString() + ' Successfully Flagged';
      _expect(fresult.metadata.success).to.equal(true);
      _expect(fresult.data[0]).to.have.property('message');
      _expect(fresult.data[0].message).to.equal(message_to_validate);
      done();
    });
    it('should successfully add the reporter to the flagged_reporter array', function(done){
      _expect(fpost.flagged_reporters[0].reporter_id).to.equal(maryolin._id.toString());
      done();
    });
    it('should succesfully add a create_date to the reporters object in flagged_reporters array', function(done){
      var testdate = Date.now();
      _expect(fpost.flagged_reporters[0]).to.have.property('create_date');
      fpost.flagged_reporters[0].create_date.valueOf()
      .should.be.within(testdate.valueOf() -10, testdate.valueOf()+10);
      done();
    });
    it('should update the is_flagged property', function(done){
      _expect(fpost.is_flagged).to.equal(true);
      done();
    });
    it('should increment the flagged_count property by 1', function(done){
      _expect(fpost.flagged_count).to.equal(1);
      done();
    });
    it('need to add a few more tests...', function(done){
      _assert.ok(false, 'Need to add test for 5 flags');
      _assert.ok(false, 'Need to add test for status change');
      _assert.ok(false, 'Need to add test for if you have already flagged');
      _assert.ok(false, 'Need to add test to ensure post does return in reg search results');
      done();
    });
  });
  describe('Testing Updating a Post', function(done){
    it('should update the text field', function(done){
      _assert.ok(false, 'not test implemented');
      done();
    });
  });
});





















