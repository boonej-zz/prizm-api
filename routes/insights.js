var mongoose   = require('mongoose'),
    prism_home = process.env.PRISM_HOME,
    utils      = require(prism_home + 'utils'),
    logger     = require(prism_home + 'logs.js'),
    PrismError  = require(prism_home + 'error'),
    User        = mongoose.model('User'),
    Twine       = require(prism_home + 'classes/Twine'),
    Insight     = mongoose.model('Insight'),
    InsightTarget = mongoose.model('InsightTarget'),
    _ = require('underscore'),
    ObjectId    = mongoose.Schema.Types.ObjectId;

var ServerError = function(res){
  utils.prismResponse(res, null, false, PrismError.serverError);
};

exports.createInsight = function(req, res){
  console.log('creating insight');
  console.log(req.body);
  if (req.body.creator){
    console.log('found creator');
    User.findOne(req.body.creator, function(err, user){
      if (err) {
        console.log(err);
        utils.prismResponse(res, null, false, PrismError.invalidRequest);
      }
      var insight = new Insight({
        creator: user._id,
        create_date: Date.now(),
        text: req.body.text,
        title: req.body.title
      });
      if (req.body.file_path && req.body.file_path != 'undefined'){
        insight.file_path = req.body.file_path;
      }
      if (req.body.text && req.body.text != 'undefined'){
        insight.file_path = req.body.file_path;
      }
      if (req.body.link && req.body.link != 'undefined'){
        insight.link = req.body.link;
      }
      if (req.body.link_title && req.body.link_title != 'undefined'){
        insight.link_title = req.body.link_title;
      }
      insight.save(function(error, insight){
        if(error){
          console.log(error);
          utils.prismResponse(res, null, false, PrismError.invalidUserRequest);
        } 
        res.send('success');

      });
    });
  } else {
    utils.prismResponse(res, null, false, PrismError.invalidUserRequest);
  }
};

exports.fetchInsights = function(req, res) {
  console.log('fetching insights');
    criteria = {};
  new Twine('Insight', criteria, req, null, function(error, result){
    if (error) {
      logger.log('error', 'Fetching insights returned error: ', {err: error});
      ServerError(res);
    } else {
      utils.prismResponse(res, result, true);
    }
  });   
};

exports.fetchUserInsights = function(req, res) {
  console.log('fetching user insights');
  var uid = req.params.id;
  var $res = res;
  User.findOne({_id: uid}, function(err, user){
    if (err) {
      res.send('error');
    } else {
      console.log('found user');
      var criteria = {target: user._id};
      new Twine('InsightTarget', criteria, req, null, function(error, result){
        if (error) {
          console.log(error);
          logger.log('error', 'Fetching user insights returned error', {err: error});
          ServerError(res);
        } else {
          utils.prismResponse($res, result, true);
        }
      }); 
    }

  });
};

exports.sendInsight = function(req, res) {
  console.log('sending insight');
  console.log(req.body);
  var insightId = req.params.id;
  var userList = req.body.users;
  var uid = req.body.uid;
  if (insightId && uid){
    console.log('searching for insight');
    var criteria = {_id: insightId};
    console.log(uid);
    Insight.findOne(criteria, function(err, insight){
      if (err) {
        console.log(err);
      } else {
        User.findOne({_id: uid}, function(err, user){
          if (err){
            console.log(err);
          } else {
            console.log('found user');
            var it = new InsightTarget({
              target: user._id,
              insight: insight._id,
              creator: insight.creator,
              file_path: insight.file_path
            });
            user.insight_count += 1;
            user.save();
            it.save(function(err, result){
              if (err) {
                console.log('there was a problem');
              } else {
                utils.registerInsightEvent(it.target, it.creator, 'insight',
                  it._id);
                console.log('target saved');
              }
            });
          }
        });
      }
    });
    res.send('done');
  }
};

exports.likeInsight = function(req, res){
  if (req.params.id){
    console.log('liking insight');
    var it_id = req.params.id;
    InsightTarget.findOne({_id: it_id})
    .populate('insight')
    .exec(function(err, it){
      if (err) {
        console.log(err);
        res.send(error);
      } else {
        console.log('liking insight');
        console.log(it);
        if (! it.liked) {
          it.liked = true;
          User.findOne({_id: it.target}, function(err, user){
            if (user.insight_count > 0) {
              user.insight_count -= 1;
              user.save();
              Insight.findOne({_id: it.insight}, function(err, insight){
                insight.likes_count += 1;
                if (it.disliked) {
                  it.disliked = false;
                  insight.dislikes_count -= 1;
                }
                insight.save();
                it.save(function(err, result){
                  console.log('saved');
                  it.insight.save(); 
                  res.send('success');
                });

              });
            }
          });
          
                  } else {
          res.send('already like');
        }
      }
    });
  } else {
    res.send('error');
  }
};

exports.dislikeInsight = function(req, res){
  if (req.params.id){
    console.log('disliking insight');
    var it_id = req.params.id;
    InsightTarget.findOne({_id: it_id})
      .populate('insight')
      .exec(function (err, it){
        if (! it.disliked) {
          it.disliked = true;
          it.insight.dislikes_count = it.insight.dislikes_count + 1;
          if (it.target.insight_count > 0) {
            it.target.insight_count -=1;
            it.target.save();
          }

          if (it.liked){
            it.liked = false;
            it.insight.likes_count -= 1;
          }
          it.save(function(err, result){
            console.log('saved');
            it.insight.save();
            res.send('success');
          });
        } else {
          res.send('already disliked');
        }
      });
  } else {
    res.send('error');
  }
}


