/**
 * Explore Route Unit Tests
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
    _user_route   = require(_prism_home + 'routes/users'),
    _t_helpers    = require(_prism_home + 'test/test_helpers'),
    Post          = require(_prism_home + 'models/post').Post,
    User          = require(_prism_home + 'models/user').User;

describe('Explore Route Unit Tests', function(done){
  var test_date = Date.now() + 9 * 60 * 60 * 1000;
  test_date = new Date(test_date);
  var fetch_url = 'https://localhost:3000/explore?feature_identifier=' + test_date.toISOString();
  var testUser = null;
  var testClient = null;
  var testCode = null;
  var testToken = null;
  var mark, edwardo, cameron, erica, sean, maryolin;
  var post1, post2, post3, post4, post5, post6;

  var executeLikePost = function(post, creator, cb){
    Post.findOne({_id: post}, function(err, result){
      if(err) throw err;
      result.likes.push({_id: creator});
      result.likes_count++;
      result.save(function(err, saved){
        if(err) throw error;
        if(cb) cb(true);
      });
    });
  };

  var setupPostsAndLikes = function(cb){
    var posts = [
      {
        text: 'test test tes1',
        hash_tags: ['foo', 'bar', 'foozbar', 'thebar'],
        hash_tags_count: 4,
        creator: mark._id,
        target_id: testUser._id,
        category: 'experiences',
        create_date: Date.now() - 10 * 60 * 1000
      },
      {
        text: 'test test tes2',
        hash_tags: ['#foo', '#bar', '#foozbar'],
        hash_tags_count: 3,
        creator: edwardo._id,
        target_id: testUser._id,
        category: 'experiences',
        create_date: Date.now() - 60 * 60 * 100
      },
      {
        text: 'test test tes3',
        hash_tags: ['fooz', '#bar'],
        hash_tags_count: 2,
        creator: sean._id,
        target_id: testUser._id,
        category: 'experiences',
        create_date: Date.now() + 60 * 60 * 1000
      },
      {
        text: 'test test tes4',
        hash_tags: ['#singularity'],
        hash_tags_count: 1,
        creator: cameron._id,
        target_id: testUser._id,
        category: 'experiences',
        create_date: Date.now() + 1 * 60 * 60 * 1000
      },
      {
        text: 'test test tes5',
        hash_tags: ['#hashtagz', 'singular', '#realtalk'],
        hash_tags_count: 3,
        creator: erica._id,
        target_id: testUser._id,
        category: 'experiences',
        create_date: Date.now() + 45 * 60 * 1000
      },
      {
        text: 'test test tes6',
        hash_tags: ['#ballin', '#rkelly'],
        hash_tags_count: 2,
        creator: mark._id,
        target_id: testUser._id,
        category: 'experiences',
        create_date: Date.now() + 10 * 60 * 1000
      },
      {text: 'test test tes7', creator: mark._id, target_id: testUser._id, category: 'experiences',create_date: Date.now() + 9 * 60 * 1000},
      {text: 'test test tes8', creator: mark._id, target_id: testUser._id, category: 'experiences',create_date: Date.now() + 8 * 60 * 1000},
      {text: 'test test tes9', creator: mark._id, target_id: testUser._id, category: 'experiences',create_date: Date.now() + 7 * 60 * 1000}
    ];
    Post.create(posts, function(err, p1, p2, p3, p4, p5, p6){
      if(err) throw err;
      post1 = p1;
      post2 = p2;
      post3 = p3;
      post4 = p4;
      post5 = p5;
      post6 = p6;

      //setup post with most likes (6) for post4
      post4.likes.push({_id: mark._id});
      post4.likes.push({_id: edwardo._id});
      post4.likes.push({_id: cameron._id});
      post4.likes.push({_id: erica._id});
      post4.likes.push({_id: maryolin._id});
      post4.likes.push({_id: sean._id});
      post4.likes_count = post4.likes_count + 6;
      post4.save(function(err, result){
        if(err) throw err;
        //setup post with 2 second most likes (4) for post1
        post1.likes.push({_id: mark._id});
        post1.likes.push({_id: edwardo._id});
        post1.likes.push({_id: sean._id});
        post1.likes.push({_id: cameron._id});
        post1.likes_count = post1.likes_count + 4;
        post1.save(function(err, post1_result){
          if(err) throw err;
          //setup post with 3rd most likes (2) for post6
          post6.likes.push({_id: edwardo._id});
          post6.likes.push({_id: sean._id});
          post6.likes_count = post6.likes_count + 2;
          post6.save(function(err, post6_result){
            //setup last 3 posts with 1 like each - post2, post3, & post5
            post5.likes.push({_id: maryolin._id});
            post5.likes_count = 1;
            post5.save(function(err, sp5){
              if(err) throw err;
              post2.likes.push({_id: maryolin._id});
              post2.likes_count = 1;
              post2.save(function(err, sp2){
                if(err) throw err;
                  post3.likes.push({_id: maryolin._id});
                  post3.likes_count = 1;
                  post3.save(function(err, sp3){
                    if(err) throw err;
                    cb();
                  });
                });
              });
            });
          });
        });
      });
    // });
  };

  var executeExploreRequestWithQueryString = function(query_string, cb){
    _request({
      method: 'GET',
      url: 'https://localhost:3000/explore' + query_string,
      json: true,
      strictSSL: false,
      headers: {"Authorization": "Bearer " + testToken.access_token}
    }, function(err, result){
      if(cb) cb(err, result.body);
    });
  };

  before(function(done){
    _t_helpers.destroyTestPost(function(){
      _t_helpers.destroyTestUser(function(){
        _t_helpers.createTestUser(function(user){
          testUser = user;
          _t_helpers.createTestToken(function(token, code, client){
            testToken = token;
            testCode = code;
            testClient = client;

            var social_users = _t_helpers.fetchFakeUsersArray();
            User.create(social_users, function(err, m, e, c, e2, s, m2){
              if(err) throw err;
              mark = m;
              edwardo = e;
              cameron = c;
              erica = e2;
              sean = s;
              maryolin = m2;

              setupPostsAndLikes(function(){
                done();
              });
            });
          });
        });
      });
    });
  });

  after(function(done){
    _t_helpers.destroyTestUser(function(){
      done();
    });
  });
  describe('Testing /explore hash_tags search', function(done){
    it('should find posts that have the exact char set in the search string', function(done){
      executeExploreRequestWithQueryString('?hash_tags=singularity', function(err, result){
        _expect(result.data.length).to.equal(1);
        _expect(result.data[0].hash_tags[0]).to.equal('#singularity');
        _expect(result.data[0]._id.toString()).to.equal(post4._id.toString());
        done();
      });
    });
    it('should find posts that have `like` char set ordering in the search string', function(done){
      _assert.ok(false, 'Test not implemented yet');
    });
  });
  describe('Testing /explore by popular posts', function(done){
    it('should return sorted by likes# desc vi sort_field', function(done){
      executeExploreRequestWithQueryString('?sort_field=likes_count', function(err,result){
        _expect(result.data[0]._id.toString()).to.equal(post4._id.toString());
        _expect(result.data[0].likes_count).to.equal(6);
        _expect(result.data[0].likes.length).to.equal(6);
        _expect(result.data[1]._id.toString()).to.equal(post1._id.toString());
        _expect(result.data[1].likes_count).to.equal(4);
        _expect(result.data[1].likes.length).to.equal(4);
        _expect(result.data[2]._id.toString()).to.equal(post6._id.toString());
        _expect(result.data[2].likes_count).to.equal(2);
        _expect(result.data[2].likes.length).to.equal(2);
        _expect(result.data[3]._id.toString()).to.equal(post3._id.toString());
        _expect(result.data[3].likes_count).to.equal(1);
        _expect(result.data[3].likes.length).to.equal(1);
        done();
      });
    });
  });
  it('should return sorted by likes and by create_date (default)', function(done){
    executeExploreRequestWithQueryString('?sort_field=likes_count', function(err, result){
      //since post3 (in the setup process) has an hour increased create_date
      //timestamp yet was LIKED after post5, we should still see post3 sorted in
      //the result index before post5. post5 should be the next result after that
      _expect(result.data[3]._id.toString()).to.equal(post3._id.toString());
      _expect(result.data[4]._id.toString()).to.equal(post5._id.toString());
      done();
    });
  });
  it('should return sorteqd by likes and create_date greater than the fi date', function(done){
    var date = Date.now() + 40 * 60 * 1000;
    date = new Date(date);

    executeExploreRequestWithQueryString('?sort_field=likes_count&feature_identifier=' + date.toUTCString(),function(err, result){
      //since post3 & post5 are technically created 45-60m in the future, we should see
      //post3 & post5 as the 1 and 2 index returned when the feature_identifier is
      //casted 40m into the feature.
      _expect(result.data[1]._id.toString()).to.equal(post3._id.toString());
      _expect(result.data[2]._id.toString()).to.equal(post5._id.toString());
      done();
    });
  });
  describe('Testing /explore fetching all posts for explore', function(done){
    it('should return all public posts sorted by a desc create_date', function(done){
      var auth_headers = 'Bearer ' + testToken.access_token;
      var posts = _t_helpers.fetchFakePostsArray(testUser, testUser);
      Post.create(posts, function(error, postsCreated){
        if(error) throw error;
        _request({
          method: 'GET',
          url: fetch_url,
          json: true,
          strictSSL: false,
          headers: {"Authorization":auth_headers}
        }, function(err, expl){
          _assert.ok(false, 'Need to actually fill the testcases for this');
          done();
        });
      });
    });
  });
});





