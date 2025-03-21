-module(worker).
-export([start/6]).

-define(left, ["admiring", "adoring", "affectionate", "agitated", "amazing", "angry", 
"awesome", "beautiful", "blissful", "bold", "boring", "brave", "busy", "charming", 
"clever", "compassionate", "competent", "condescending", "confident", "cool", "cranky", 
"crazy", "dazzling", "determined", "distracted", "dreamy", "eager", "ecstatic", "elastic", 
"elated", "elegant", "eloquent", "epic", "exciting", "fervent", "festive", "flamboyant", 
"focused", "friendly", "frosty", "funny", "gallant", "gifted", "goofy", "gracious", 
"great", "happy", "hardcore", "heuristic", "hopeful", "hungry", "infallible", "inspiring", 
"intelligent", "interesting", "jolly", "jovial", "keen", "kind", "laughing", "loving", 
"lucid", "magical", "modest", "musing", "mystifying", "naughty", "nervous", "nice", 
"nifty", "nostalgic", "objective", "optimistic", "peaceful", "pedantic", "pensive", 
"practical", "priceless", "quirky", "quizzical", "recursing", "relaxed", "reverent", 
"romantic", "sad", "serene", "sharp", "silly", "sleepy", "stoic", "strange", "stupefied", 
"suspicious", "sweet", "tender", "thirsty", "trusting", "unruffled", "upbeat", "vibrant", 
"vigilant", "vigorous", "wizardly", "wonderful", "xenodochial", "youthful", "zealous", "zen"]).

-define(right, ["agnesi", "albattani", "allen", "almeida", "antonelli", "archimedes", 
"ardinghelli", "aryabhata", "austin", "babbage", "banach", "banzai", "bardeen", "bartik", 
"bassi", "beaver", "bell", "benz", "bhabha", "bhaskara", "black", "blackburn", "blackwell", 
"bohr", "booth", "borg", "bose", "bouman", "boyd", "brahmagupta", "brattain", "brown", 
"buck", "burnell", "cannon", "carson", "cartwright", "carver", "cerf", "chandrasekhar", 
"chaplygin", "chatelet", "chatterjee", "chaum", "chebyshev", "clarke", "cohen", "colden", 
"cori", "cray", "curie", "curran", "darwin", "davinci", "dewdney", "dhawan", "diffie", 
"dijkstra", "dirac", "driscoll", "dubinsky", "easley", "edison", "einstein", "elbakyan", 
"elgamal", "elion", "ellis", "engelbart", "euclid", "euler", "faraday", "feistel", "fermat", 
"fermi", "feynman", "franklin", "gagarin", "galileo", "galois", "ganguly", "gates", "gauss", 
"germain", "goldberg", "goldstine", "goldwasser", "golick", "goodall", "gould", "greider", 
"grothendieck", "haibt", "hamilton", "haslett", "hawking", "hellman", "hermann", "herschel", 
"hertz", "heyrovsky", "hodgkin", "hofstadter", "hoover", "hopper", "hugle", "hypatia", 
"ishizaka", "jackson", "jang", "jemison", "jennings", "jepsen", "johnson", "joliot", "jones", 
"kalam", "kapitsa", "kare", "keldysh", "keller", "kepler", "khayyam", "khorana", "kilby", 
"kirch", "knuth", "kowalevski", "lalande", "lamarr", "lamport", "leakey", "leavitt", 
"lederberg", "lehmann", "lewin", "lichterman", "liskov", "lovelace", "lumiere", "mahavira", 
"margulis", "matsumoto", "maxwell", "mayer", "mccarthy", "mcclintock", "mclaren", "mclean", 
"mcnulty", "meitner", "mendel", "mendeleev", "meninsky", "merkle", "mestorf", "mirzakhani", 
"montalcini", "moore", "morse", "moser", "murdock", "napier", "nash", "neumann", "newton", 
"nightingale", "nobel", "noether", "northcutt", "noyce", "panini", "pare", "pascal", "pasteur", 
"payne", "perlman", "pike", "poincare", "poitras", "proskuriakova", "ptolemy", "raman", 
"ramanujan", "rhodes", "ride", "ritchie", "robinson", "roentgen", "rosalind", "rubin", "saha", 
"sammet", "sanderson", "satoshi", "shamir", "shannon", "shaw", "shirley", "shockley", "shtern", 
"sinoussi", "snyder", "solomon", "spence", "stonebraker", "sutherland", "swanson", "swartz", 
"swirles", "taussig", "tesla", "tharp", "thompson", "torvalds", "tu", "turing", "varahamihira", 
"vaughan", "villani", "visvesvaraya", "volhard", "wescoff", "wilbur", "wiles", "williams", 
"williamson", "wilson", "wing", "wozniak", "wright", "wu", "yalow", "yonath", "zhukovsky"]).

start(Name, Main, Module, Id, Sleep, Jitter) ->
    spawn(fun() -> init(Name, Main, Module, Id, Sleep, Jitter) end).

init(Name, Main, Module, Id, Sleep, Jitter) ->
    Cast = apply(Module, start, [Id, self(), Jitter]),
    Main ! {join, self(), Cast},
    receive
        {peers, Peers} ->
            Cast ! {peers, Peers},

            Wait = if Sleep == 0 -> 0; true -> rand:uniform(Sleep) end,
            {ok, Tref} = timer:send_after(Wait, new_topic),
            worker(Name, Id, Cast, 1, 1, Sleep, Tref),
            Cast ! stop
    end.
    
worker(Name, Id, Cast, NextSnd, NextRcv, Sleep, Tref) ->
    receive
        {deliver, {FromName, From, Msg, SenderN, Nre}} ->
            io:format("~s RECV(~4w) ; From: ~s(~4w) ; Subject: ~s~n", 
                      [Name, NextRcv, FromName, SenderN, Msg]),
            if
                From == Id ->
                    worker(Name, Id, Cast, NextSnd, NextRcv+1, Sleep, Tref);
                true ->
                    Op = rand:uniform(),
                    if (Op >= 0.8) and (Tref /= null) and (Nre < 3) ->
                          cast_response(Name, Id, Cast, Msg, NextSnd, Nre+1),
                          worker(Name, Id, Cast, NextSnd+1, NextRcv+1, Sleep, Tref);
                       true ->
                          worker(Name, Id, Cast, NextSnd, NextRcv+1, Sleep, Tref)
                    end   
            end;
        new_topic ->
            cast_new_topic(Name, Id, Cast, NextSnd),
            Wait = if Sleep == 0 -> 0; true -> rand:uniform(Sleep) end,
            {ok, NewTref} = timer:send_after(Wait, new_topic),
            worker(Name, Id, Cast, NextSnd+1, NextRcv, Sleep, NewTref);
        pause ->
            timer:cancel(Tref),
            worker(Name, Id, Cast, NextSnd, NextRcv, Sleep, null);
        resume ->
            Wait = if Sleep == 0 -> 0; true -> rand:uniform(Sleep) end,
            {ok, NewTref} = timer:send_after(Wait, new_topic),
            worker(Name, Id, Cast, NextSnd, NextRcv, Sleep, NewTref);
        stop ->
            ok;
        Error ->
            io:format("strange message: ~w~n", [Error]),
            worker(Name, Id, Cast, NextSnd, NextRcv, Sleep, Tref)
    end.

cast_new_topic(Name, Id, Cast, N) ->
    L = lists:nth(rand:uniform(length(?left)), ?left),
    R = lists:nth(rand:uniform(length(?right)), ?right),
    Sbj = L ++ " " ++ R,
    Msg = {Name, Id, Sbj, N, 0},
    io:format("~s SEND(~4w) ; Subject: ~s~n", [Name, N, Sbj]),
    Cast ! {send, Msg}.
 
cast_response(Name, Id, Cast, Text, N, Nre) ->
    Sbj = "Re:" ++ Text,
    Msg = {Name, Id, Sbj, N, Nre},
    io:format("~s SEND(~4w) ; Subject: ~s~n", [Name, N, Sbj]),
    Cast ! {send, Msg}.
