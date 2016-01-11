var express = require('express');
var app = express();
var mongoose = require('mongoose');
var config = require('config');
var gateway = require('../gateway');
var Organization = mongoose.model('Organization');
var User = mongoose.model('User');
var Message = mongoose.model('Message');
var Group = mongoose.model('Group');
var ObjectId = mongoose.Types.ObjectId;
var _ = require('underscore');

// Organization Endpoints

app.get('/', gateway, function(req, res){
  Organization.find({}, function(err, orgs){
    res.status(200).json(orgs);
  });
});

/**
 * @api {get} /organizations/:oid Get Organization
 * @apiName GetOrganization
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiUse Error
 **/
app.get('/:oid', gateway, function(req, res){
  var oid = req.params.oid;
  Organization.findOneAndFlatten(oid, function(err, org){
    if (err) res.status(500).json(err);
    else res.status(200).json(org);
  });
});

/**
 * @api {get} /organizations/:oid/groups Get Groups
 * @apiName GetGroups
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam (Query) {String} [requestor] Unique id for requestor
 * @apiUse Error
 **/
app.get('/:org_id/groups', gateway, function(req, res) {
  console.log('in request');
  var uid = req.query.requestor;
  var org_id = req.params.org_id;
  if (org_id) {
    Organization.findOne({_id: org_id}, function(err, org){
      if (org) {
        org.fetchGroups(function(err, groups){
          if (err) {
            console.log(err);
            res.status(500).json(err);
          } else {
            var result = [];
            _.each(groups, function(g){
              g = g.toObject();
              var muted = false;
              _.each(g.mutes, function(mute){
                if (String(uid) == String(mute)){
                  muted = true;
                }
              });
              g.muted = muted;
              result.push(g);
            });
            res.status(200).json(result);
          }
        });
      } else {
        console.log(err);
        res.status(400).send();
      }
    });
    
  } else {
    res.status(400).send();
  }
});

/**
 * @api {get} /organizations/:oid/users/:uid/unread Get Unread Counts
 * @apiName GetGroups
 * @apiDescription Get a top level message count for a user in the organization.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} uid Unique ID for user
 * @apiUse Error
 **/
app.get('/:oid/users/:uid/unread', gateway, function(req, res) {
  var uid = req.params.uid;
  var oid = req.params.oid;
  User.findOne({_id: uid}, function(err, user){
    if (user) {
      if (user.type == 'institution_verified') {
        Organization.findOne({owner: user._id})
        .populate({path: 'groups', model: 'Group'})
        .exec(function(err, org){
          Message.getTopLevelMessageCount(org, user, function(err, results){
            if (err) {
              res.status(500).json(err);
            } else {
              res.status(200).json(results);
            }
          });
        });
      } else {
        User.populate(user, {path: 'org_status.groups', model: 'Group'}, function(err, user){
          var org = _.filter(user.org_status, function(obj){
            return String(obj.organization) == String(oid);
          });
          if (_.isArray(org)) {
            org = org[0];
          }
          Message.getTopLevelMessageCount(org, user, function(err, results){
            if (err) {
              res.status(500).json(err);
            } else {
              res.status(200).json(results);
            }
          });
        });

      }
    }
  });
});

/**
 * @api {get} /organizations/:oid/users/:uid/groups Get Groups For User
 * @apiName GetGroupsForUser
 * @apiDescription Get a list of all groups that a user belongs to in the 
 *  context of a single organization.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} uid Unique ID for user
 * @apiUse Error
 **/
app.get('/:org_id/users/:uid/groups', gateway, function(req, res) {
  var uid = req.params.uid;
  var org_id = req.params.org_id;
  if (uid && org_id) {
    User.findOne({_id: uid}, function(err, user) {
      if (user) {
        user.fetchGroups(org_id, function(err, groups){
          if (err) {
            console.log(err);
            res.status(500).json(err);
          } else {
            res.status(200).json(groups);
          }
        });
      } else {
        res.status(400).send();
      }
    });
  } else {
    res.status(400).send();
  }
});


var getMessages = function (criteria, requestor, limit, req, res) {
 Message.findAndFlatten(criteria, requestor, limit, function(err, messages){
   console.log('completed request');
    if (err) {
      res.status(500).json(err);
    } else {
      messages = Message.isLiked(messages, requestor); 
      res.status(200).json(messages);
    }
  });
};

/** GROUPS **/

/** CREATE **/
/**
 * @api {post} /organizations/:oid/groups Create Group
 * @apiName CreateGroup
 * @apiDescription Creates a group within an organization.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam (Body) {String} name Name of group
 * @apiParam (Body) {String} description Description of group
 * @apiParam (Body) {String} organization Unique id for organization
 * @apiParam (Body) {String} [leader] Unique id for group leader (user)
 * @apiUse Error
 **/
app.post('/:oid/groups', gateway, function(req, res) {
  var oid = req.params.oid;
  var params = {
    name: req.body.name,
    description: req.body.description,
    organization: oid,
    leader: req.body.leader
  };
  var members = req.body.members;
  Group.newGroup(params, function(err, group) {
    if (err) {
      res.status(500).json(err);
    } else {
      if (!_.isArray(members)) {
        members = JSON.parse(members);
      }
      if (params.leader) {
        User.addToGroup(params.leader, group);
      }
      _.each(members, function(m){
        User.addToGroup(m, group);
      });
    }
    res.status(200).send(group);

  });
  
});

/** UPDATE **/
/**
 * @api {put} /organizations/:oid/groups/:gid Update Group
 * @apiName UpdateGroup
 * @apiDescription Update a group's information
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} gid Unique ID for group
 * @apiParam (Body) {String} name Name of group
 * @apiParam (Body) {String} description Description of group
 * @apiParam (Body) {String} organization Unique id for organization
 * @apiParam (Body) {String} leader Unique id for group leader (user)
 * @apiParam (Body) {String[]} members A list of user ids that belong to the group
 * @apiUse Error
 **/
app.put('/:oid/groups/:gid', gateway, function(req, res) {
  var oid = req.params.oid;
  var gid = req.params.gid;
  var name = req.body.name;
  var leader = req.body.leader;
  var members = req.body.members;
  console.log(gid);
  if (!_.isArray(members)) {
    members = JSON.parse(members);
  }

  Group.findOne({_id: gid}, function(err, group) {
    if (err || !group) {
      res.status(400).json(err);
    } else {
      group.name = name;
      group.leader = leader;
      group.description = req.body.description;
      group.save(function(err, g){
        User.find({active: true, org_status: {
          $elemMatch: {
            organization: ObjectId(oid),
            groups: ObjectId(gid) 
          }
        }
        }, function(err, users){
          if (users){
            _.each(users, function(u) {
              var idx = -1;
              _.each(members, function(m, i) {
                if (String(m) == String(u._id)){
                  idx = i;
                }
              });
              if (idx == -1){
                _.each(u.org_status, function(o){
                  var i = 0;
                  if (String(o.organization) == String(oid)){
                    _.each(o.groups, function(g, index) {
                      if (String(g) == String(gid)) {
                        i = index;
                      }
                    });
                    o.groups.splice(i, 1);
                  }
                });
                u.markModified('org_status');
                u.save(function(err, result){
                  if (err) console.log(err);
                });
              }
            });
            
          } 
          _.each(members, function(m) {
              User.addToGroup(m, gid);
          });
          res.status(200).json(g);
          
        });
      });
    }
  });
});

// Add Mute to group
/**
 * @api {post} /organizations/:oid/groups/:gid/mutes Create Mute
 * @apiName Create Mute
 * @apiDescription Allows a user to opt out of push notifications within a 
 *  group.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} gid Unique ID for group
 * @apiParam (Body) {String} user Unique ID for user
 * @apiUse Error
 **/
app.post('/:oid/groups/:gid/mutes', gateway, function(req, res) {
  var oid = req.params.oid;
  var gid = req.params.gid;
  var uid = req.body.user;
  if (gid == 'all') {
    Organization.mute(oid, uid, function(err, org){
      User.findOneCore(uid, function(err, user) {
        if (err) res.status(500).json(err);
        else res.status(200).json(user);
      });
    });
  } else {
    Group.mute(gid, uid, function(err, group) {
      if (err) res.status(500).json(err);
      else res.status(200).json(group);
    });
  }
});

// Remove Mute from group
/**
 * @api {delete} /organizations/:oid/groups/:gid/mutes/:uid Create Mute
 * @apiName Create Mute
 * @apiDescription Allows a user to resume push notifications within a 
 *  group.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} gid Unique ID for group
 * @apiParam {String} uid Unique ID for user
 * @apiUse Error
 **/
app.delete('/:oid/groups/:gid/mutes/:uid', gateway, function(req, res){
  var gid = req.params.gid;
  var uid = req.params.uid;
  var oid = req.params.oid;
  if (gid == 'all') {
    Organization.unmute(oid, uid, function(err, org){
      User.findOneCore(uid, function(err, user) {
        if (err) res.status(500).json(err);
        else res.status(200).json(user);
      });
    });
  } else {
    Group.unmute(gid, uid, function(err, group){
      if (err) res.status(500).json(err);
      else res.status(200).json(group);
    });
  }
});

/**
 * @api {get} /organizations/:oid/groups/:gid/messages Get Messages
 * @apiName GetMessages
 * @apiDescription Fetches messages within the specified group.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} gid Unique ID for group
 * @apiParam (Query) {String} requestor Unique ID for user requesting
 * @apiParam (Query) {Date} [before] Get messages that came before specified date
 * @apiParam (Query) {Date} [after] Get messages that came after specified date
 * @apiParam (Query) {Integer} [limit=15] Limit results to the count
 * @apiUse Error
 **/
app.get('/:org_id/groups/:gid/messages', gateway, function(req, res) {
  var org_id = req.params.org_id;
  var gid = req.params.gid != "all"?req.params.gid:null;
  
  var before = req.query.before?new Date(req.query.before):false;
  var after = req.query.after?new Date(req.query.after):false;
  var limit = req.query.limit || false;
  var requestor = req.query.requestor || false;
  var params = {organization: org_id, target: null};
  
  var createDateBefore = false;
  var createDateAfter = false;
  if (after) {
    createDateAfter = {$gt: after};
  }
  if (before) {
    createDateBefore = {$lt: before};
  }
  var createDateRequest = false;
  if (before) {
    if (after) {
      createDateRequest = {$and: [createDateBefore, createDateAfter]}; 
    } else {
      createDateRequest = createDateBefore;
    }
  } else if (after) {
    createDateRequest = createDateAfter;
  }

  if (createDateRequest) {
    params.create_date = createDateRequest;
  }
  if (gid && gid.substr(0, 1) == "~") {
    Group.findOne({organization: org_id, name: gid.substr(1)})
      .select({_id: 1})
      .exec(function(err, group){
        params.group = String(group._id);
        if (err) {
          res.status(500).json(err);
        } else {
          getMessages(params, requestor, limit, req, res);
        }
    });
  } else {
    params.group = gid;
    getMessages(params, requestor, limit, req, res); 
  }

 });

/**
 * @api {post} /organizations/:oid/groups/:gid/messages Create Message
 * @apiName GetMessages
 * @apiDescription Creates a message within the specified group.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} gid Unique ID for group
 * @apiParam (Body) {String} creator Unique ID for message creator
 * @apiParam (Body) {String} [image_url] Path to message image url
 * @apiParam (Body) {String} [text] Text content of message
 * @apiUse Error
 **/
app.post("/:oid/groups/:gid/messages", gateway, function(req, res){
  var oid = req.params.oid;
  var gid = req.params.gid;
  if (gid == "all") {
    gid = null;
  }
  var text = req.body.text;
  var image_url = req.body.image_url;
  var creator = req.body.creator;
  Message.createMessage({
    organization: oid,
    group: gid,
    text: text,
    image_url: image_url,
    creator: creator
  }, function(err, message){
    if (err) {
      res.status(500).json(err);
    } else {
      res.status(200).json([message]);
    }
  });
})

/**
 * @api {post} /organizations/:oid/groups/:gid/messages/:mid Like Message
 * @apiName Like Message
 * @apiDescription Creates a message within the specified group.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} gid Unique ID for group
 * @apiParam {String} mid Unique ID for message
 * @apiParam (Body) {String} requestor Unique ID for like requestor
 * @apiUse Error
 **/
app.post("/:oid/groups/:gid/messages/:mid", gateway, function(req, res){
  var mid = req.params.mid;
  var requestor = req.body.requestor;
  Message.likeMessage(mid, requestor, function(err, message){
    if (err) res.status(500).json(err);
    else {
      res.status(200).json(message);
    }
  });
});

/**
 * @api {get} /organizations/:oid/groups/:gid/messages/:mid/read Get Message Readers
 * @apiName GetMessageReaders
 * @apiDescription Gets a list of users who have read a message.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} gid Unique ID for group
 * @apiParam {String} mid Unique ID for message
 * @apiUse Error
 **/
app.get("/:oid/groups/:gid/messages/:mid/read", gateway, function(req, res) {
  var mid = req.params.mid;
  Message.fetchRead(mid, function(err, users){
    if (err) res.status(500).json(err);
    else res.status(200).json(users);
  });
});

/**
 * @api {get} /organizations/:oid/groups/:gid/messages/:mid/likes Get Message Likes
 * @apiName GetMessageLikes
 * @apiDescription Gets a list of users who have liked a message.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} gid Unique ID for group
 * @apiParam {String} mid Unique ID for message
 * @apiUse Error
 **/
app.get("/:oid/groups/:gid/messages/:mid/likes", gateway, function(req, res) {
  var mid = req.params.mid;
  Message.fetchLikes(mid, function(err, users){
    if (err) res.status(500).json(err);
    else res.status(200).json(users);
  });
});

/**
 * @api {get} /organizations/:oid/users/:uid/messages Get Direct Messages
 * @apiName GetDirectMessages
 * @apiDescription Gets a list of messages passed directly beteween organization
 *  users.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} uid Unique ID for user
 * @apiParam (Query) {String="digest"} [format] Digest format returns user and counts only
 * @apiParam (Query) {String} [target] Unique id for targeted user - required if format is missing
 * @apiParam (Query) {Date} [before] Fetch all messages before this date
 * @apiParam (Query) {Date} [after] Fetch all messages after this date
 * @apiUse Error
 **/
app.get("/:oid/users/:uid/messages", gateway, function(req, res){
  var oid = req.params.oid;
  var uid = req.params.uid;
  var format = req.query.format;
  var target = req.query.target;
  var before = req.query.before;
  var after = req.query.after;
  if (format == 'digest') {
    User.findOrganizationUser(uid, oid, function(err, user){
      if (err) {
        res.status(500).json(err);
      } else {
        Message.getMessageAggregate(user, oid, function(err, users){
          if (err) {
            res.status(500).json(err);
          } else {
            res.status(200).json(users);
          }
        });
      }
    });
  } else {
    Message.fetchDirectMessages(uid, target, oid, before, after, function(err, messages){
      if (err) {
        res.status(500).json(err);
      } else {
        res.status(200).json(messages);
      }
    });
  }
});

/**
 * @api {post} /organizations/:oid/users/:uid/messages Create Direct Message
 * @apiName CreateDirectMessage
 * @apiDescription Creates a targeted message to another organization member.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} uid Unique ID for user sending message
 * @apiParam (Body) {String} target Unique id for targeted user
 * @apiParam (Body) {String} [text] Text content of message
 * @apiParam (Body) {String} [image_url] Path to image content of message
 * @apiUse Error
 **/
app.post("/:oid/users/:uid/messages", gateway, function(req, res){
  var oid = req.params.oid;
  var uid = req.params.uid;
  var target = req.query.target;
  Message.createMessage({
    organization: oid,
    target: req.body.target,
    text: req.body.text,
    image_url: req.body.image_url,
    creator: uid
  }, function(err, message){
    if (err) {
      res.status(500).json(err);
    } else {
      res.status(200).json([message]);
    }
  });

});

/** MEMBERS **/
/**
 * @api {get} /organizations/:oid/users/:uid/contacts Get Contacts
 * @apiName Get Contacts
 * @apiDescription Gets a list of all users that the requestor has permission to contact.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} uid Unique ID for user sending message
 * @apiParam (Query) {String} [last] Name of last user pulled
 * @apiUse Error
 **/
app.get('/:oid/users/:uid/contacts', gateway, function(req, res) {
  var oid = req.params.oid;
  var uid = req.params.uid;
  var last = req.query.last;
  User.findOrganizationUser(uid, oid, function(err, user){
    if (user.type == 'institution_verified'){
      User.findOrganizationMembers(oid, last, function(err, users){
        if (err) res.status(500).json(err);
        else res.status(200).json(users);
      }, user);
    } else {
      if (user.org_status && user.org_status[0].role == 'leader') {
        User.findOrganizationMembers(oid, last, function(err, users){
          if (err) res.status(500).json(err);
          else res.status(200).json(users);
        }, user); 
      } else {
        User.findAvailableDirectRecipients(user, function(err, users){
          if (err) {
            res.status(500).json(err);
          } else {
            res.status(200).json(users);
          }
        });
      }
    }
  });
});

/**
 * @api {get} /organizations/:oid/members Get Organization Members
 * @apiName GetMembers
 * @apiDescription Gets a list of all users in an organization.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam (Query) {String} [last] Name of last user pulled
 * @apiUse Error
 **/
app.get('/:oid/members', gateway, function(req, res){
  var oid = req.params.oid;
  var last = req.query.last;
  User.findOrganizationMembers(oid, last, function(err, users){
    if (err) res.status(500).json(err);
    else res.status(200).json(users);
  });
});


/**
 * @api {get} /organizations/:oid/groups/:gid/members Get Group Members
 * @apiName GetGroupMembers
 * @apiDescription Gets a list of all users in a group.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} gid Unique ID for group
 * @apiParam (Query) {String} [last] Name of last user pulled
 * @apiParam (Query) {Boolean} [show_all] Directs the API to return all organization
 *  members along with a status message on whether they belong to the group.
 * @apiUse Error
 **/
app.get('/:oid/groups/:gid/members', gateway, function(req, res) {
  var oid = req.params.oid;
  var gid = req.params.gid;
  var last = req.query.last;
  var show_all = req.query.show_all;
  
  if (show_all) {
    User.findOrganizationMembers(oid, last, function(err, users){
      if (err) {
        res.status(500).json(err);
      } else {
        _.each(users, function(u){
          u.is_member = false;
          _.each(u.org_status[0].groups, function(g){
            if (String(g) == String(gid)){
              u.is_member = true;
            }
          });
        });
        res.status(200).json(users);
      }
    });
  } else {
    User.findGroupMembers(oid, gid, last, function(err, users){
      if (err) res.status(500).json(err);
      else {
        res.status(200).json(users);
      }
    });
  }
  
});

/**
 * @api {get} /organizations/:oid/leaderboard Get Organization Leaderboard
 * @apiName GetLeaderboard
 * @apiDescription Gets a sorted list of the point leaders in an organization.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam (Query) {Number} [limit] Limit the results to this number
 * @apiParam (Query) {Number} [skip] Skip this number of records
 * @apiUse Error
 **/
app.get('/:oid/leaderboard', gateway, function(req, res){
  
  var oid = req.params.oid;
  var limit = req.query.limit || 15;
  var skip = req.query.skip || 0;

  if (oid) {
    Organization.fetchLeaderboard(oid, limit, skip, function(err, leaders){
      if (err) {
        Error.serverError(res);
      } else {
        res.status(200).json(leaders);
      }
    });
  } else {
    Error.invalidRequest(res, 'You must provide an organization id');
  }
});

/**
 * @api {get} /organizations/:oid/leaderboard Get individual score
 * @apiName GetIndividualScore
 * @apiDescription Gets a users individual leaderboard score.
 * @apiVersion 2.0.0
 * @apiGroup Organizations
 * @apiParam {String} oid Unique ID for organization
 * @apiParam {String} uid Unique ID for user
 * @apiUse Error
 **/
app.get('/:oid/leaderboard/:uid', gateway, function(req, res) {
  
  var oid = req.params.oid;
  var uid = req.params.uid;

  if (oid && uid) {
    Organization.fetchIndividualScore(oid, uid, function(err, score) {
      if (err) {
        Error.serverError(res);
      } else {
        res.status(200).json(score);
      }
    });
  } else {
    Error.invalidRequest(res, 'You must include a user id and an organization id.');
  }

});

app.get('/:oid/users/:uid/surveys',  function(req, res) {
  
  var oid = req.params.oid;
  var uid = req.params.uid;
  var skip = req.query.skip || 0;
  var limit = req.query.limit || 15;

  Organization.fetchUserSurveys(uid, oid, limit, skip, function(err, surveys){
    if (err) {
      Error.serverError(res);
    } else {
      res.status(200).json(surveys);
    }
  });
});

module.exports = app;
