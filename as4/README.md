Communication On FaceIn
=========================
A social network implemented in Erlang with each person represented
as its own server.
by Jens Dahl Møllerhøj

This is my hand in for the assignment found at [http://www.diku.dk/~kflarsen/ap-e2014/facein/communication-facein.html]. I believe that all parts of the assignment has been solved.

The code can be compiled with `erl`.
Open `erl` and type `c(facein_spec).` and hit enter to load the program and the tests.
Type `facein_spec:tests().` to run the test suite.

The erlang compiler yields no errors when compiling.

Notes
-----
My person server has 7 functions:

- name: Get the name of the person.
- add_friend: Add a person to the list of friends.
- friends: Get a list of the persons friends.
- received_messages: Get a list of all messages received.
- broadcast: Send messages to all friends.
- receive_message: Add a message to the inbox, and broadcast the message to all friends.
- shutdown: Stop running.

The utility function send_list sends a message to a list of friends. Before doing so,
it check the following:

- That the message has not already been sent.
- That the radius is more than zero.
- That the list of friends is not empty.

Tests
-----
The test suite checks that the following is true

- A new person has 0 friends.
- We can ask for and receive the name of a person.
- We can ask for and receive the name of a person.
- A person can make a friend, and return the list of friends.
- Broadcasting with radius 2 sends to the friends of friends.
- The same message is not broadcasted twice with the same radius.

Room for improvement
--------------------
Test suite rely on a sleep function to make sure all messages has been broadcasted
before checking the test conditions. This is bad practice, because the tests might
fail if the message passing is slow.
