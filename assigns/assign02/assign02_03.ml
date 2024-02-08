(* Updating Recent Posts

   Notes: An online post (defined by the record type `post` below) has
   a title, content and timestamp.  A user is defined by a record type
   with many fields (see below) but the important fields for this
   problem are

     old_posts: a list of posts in decreasing order of timestamp
     recent_posts: a list of posts in decreasing order of timestamp

   These fields have the further property that every post in
   `old_posts` is older then every post in `recent_posts` (i.e., their
   timestamps are smaller).

   Problem:
   Implement a function `update_recent` which given

     u : a user
     time : a nonnegative integer
     stale : a nonnegative integer

   returns a new `user` with the following properties:

   * every post in `recent_post` at least `stale` timesteps old (with
   respect to the current time `time`) has been moved to `old_posts`

   * the ordering property above is maintained: timestamps still
   appear in decreasing order, and `old_post`s are older than
   `recent_post`s.

   Example:
   let _ = assert (update_recent (mk [] [p 30;p 20;p 10;p 0]) 50 30 = mk [p 20;p 10;p 0] [p 30])
   (* see below for the definition of `mk` and `p` *)

*)

type post = {
  title : string ;
  content : string ;
  timestamp : int ;
}

type user = {
  username : string ;
  email : string ;
  time_joined : int ;
  is_paid_user : bool ;
  balance : int ;
  next_payment_time : int ;
  is_paused : bool ;
  num_followers : int ;
  num_likes : int ;
  old_posts : post list ;
  recent_posts : post list ;
}

let p t = {title="";content="";timestamp=t}
let mk op rp = {
  username = "" ;
  email = "" ;
  time_joined = 0 ;
  is_paid_user = true ;
  balance=0 ;
  next_payment_time = 0;
  is_paused = true ;
  num_followers = 0 ;
  num_likes  = 0 ;
  old_posts = op;
  recent_posts = rp;
}


let update_recent (u : user) (time : int) (stale : int) : user =
      let rec aux acc1 acc2 posts = 
      match posts with
      | [] -> (List.rev acc1, List.rev acc2)
      | x :: xs -> 
        if time - (x.timestamp) >= stale then
          aux (x :: acc1) acc2 xs
        else 
          aux acc1 (x :: acc2) xs 
      
      in

          let (update_old, updated_new) = aux u.old_posts [] u.recent_posts in 
            mk (update_old) (updated_new)

