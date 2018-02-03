function updateDates() {
  var ops = [];
  var feeds = db.feeds.find({
    lastPostDate :  { '$exists':  true,  '$type' :  2 }
  });

  feeds.forEach(function(feed) {
    var date = new ISODate(feed.lastPostDate);
    ops.push({
      updateOne : {
        filter : { _id:  feed._id },
        update : { '$set':  { lastPostDate :  date } }
      }
    });

    if (ops.length  ===  500) {
      db.feeds.bulkWrite(ops);
      ops = [];
    }
  });

  if (ops.length > 0) {
    db.feeds.bulkWrite(ops);
  }
}

