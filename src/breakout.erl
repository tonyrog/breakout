%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Break out game
%%% @end
%%% Created :  4 Sep 2021 by Tony Rogvall <tony@rogvall.se>

-module(breakout).
-behaviour(epxw).

-export([start/0]).
-export([status/0]).
-export([install/0]).
-export([init/1, draw/3, key_press/2, key_release/2]).
-export([handle_info/2]).

-export([install_cmds/0]).

-define(WINDOW_WIDTH,  240).
-define(WINDOW_HEIGHT, 320).
-define(BORDER, 6).
-define(DIGIT_DOT,   4).
-define(GAME_LEFT,   ?BORDER).
-define(GAME_RIGHT,  (?WINDOW_WIDTH-?BORDER)).
-define(GAME_TOP,    ?BORDER).
-define(GAME_BOTTOM, (?WINDOW_HEIGHT-?BORDER)).
-define(GAME_WIDTH,  (?WINDOW_WIDTH-2*?BORDER)).
-define(GAME_HEIGHT, (?WINDOW_HEIGHT-2*?BORDER)).
-define(GAME_BORDER_COLOR, white).
-define(GAME_BACKGROUND_COLOR, {0,0,0}).  %% green background
-define(SCORE_OFFSET_X, (?BORDER+4)).
-define(SCORE_OFFSET_Y, (?BORDER+4)).
-define(SCORE_TEXT_COLOR, {0,0,0,255}).
-define(SCORE_COLOR, {0,0,255}).
-define(PADEL_WIDTH, 32).
-define(PADEL_HEIGHT, 8).
-define(PADEL_COLOR, {255,0,0}).
-define(PADEL_SPEED, 3).
-define(PADEL_WALL_DIST, (5*?BORDER)).
-define(BALL_RADIUS, 4).
-define(BALL_COLOR, white).
-define(BALL_SPEED_0, 2.0).
-define(GOAL_SIZE,  (3*?PADEL_HEIGHT)).
-define(PADEL_KEY_LEFT,  left).
-define(PADEL_KEY_RIGHT, right).
-define(WALL_FRICTION,    1).
-define(WALL_RESTITUTION, 1).
-define(TICK, 10).
-define(BRICKS_PER_ROW, 14).
-define(BRICK_HEIGHT, (?BORDER+3)).
-define(BRICK_WIDTH,  (?WINDOW_WIDTH div ?BRICKS_PER_ROW)).

-define(APP, ?MODULE).
-define(APPSTR, ?MODULE_STRING).
-define(DOTAPP, [$.|?APPSTR]).
-define(APPPNG, ?APPSTR++".png").
-define(APPDSK, ?APPSTR++".desktop").

%% -define(dbg(F, A), io:format((F), (A))).
-define(dbg(F, A), ok).

-define(vec_x(V), element(1,(V))).
-define(vec_y(V), element(2,(V))).

-define(rect_x(R), element(1,(R))).
-define(rect_y(R), element(2,(R))).
-define(rect_w(R), element(3,(R))).
-define(rect_h(R), element(4,(R))).
-define(set_rect_x(R,X), setelement(1,(R),(X))).
-define(set_rect_y(R,Y), setelement(2,(R),(Y))).

-record(padel,
	{
	 box :: epx:epx_rect(),
	 xdir = 0 :: integer(),
	 ydir = 0 :: integer(),
	 color :: epx:epx_color(),
	 pressed = 0 :: integer()  %% number of keys pressed 0,1,2
	}).

-record(ball,
	{
	 pos :: epx:point2d(),
	 vel = {0.0,0.0} :: {float(),float()},    %% velocity vector
	 speed = ?BALL_SPEED_0 :: float(),        %% pixels per tick
	 color = ?BALL_COLOR :: epx:epx_color(),
	 radius = ?BALL_RADIUS :: epx:dim()
	}).

-type rowmask() :: 2#00000000000000..2#11111111111111.

-record(game,
	{
	 nplayers = 1 :: 1 | 2,
	 player = 1 :: 1 | 2,   %% current player
	 score1 :: integer(),   %% current score (player 1)
	 score2 :: integer(),   %% current score (player 2)
	 padel :: #padel{},
	 ball :: #ball{},
	 brick_colors = {} :: {epx:epx_color()},
	 bricks = {} :: {rowmask()},
	 font :: epx:font(),
	 font_dim :: {Width::epx:dim(),Height::epx:dim()},
	 font_ascent :: epx:dim(),
	 plop :: {map(), binary()},
	 peep :: {map(), binary()},
	 beep :: {map(), binary()},
	 alsa :: term(),
	 alsa_options :: map()	 
	}).

status() ->
    io:format("breakout is running\n").

askpass() ->
    filename:join(code:priv_dir(epx), "epx_askpass").

%% runtime install, first time only for APPIMAGE
install() ->
    APPIMAGE = os:getenv("APPIMAGE",undefined),
    if APPIMAGE =:= undefined ->
	    ok;
       true ->
	    Home = os:getenv("HOME"),
	    {ok,Vsn} = application:get_key(?APP, vsn),
	    VsnNL = Vsn++"\n",
	    case file:read_file(filename:join([Home,?DOTAPP,"installed"])) of
		{ok,BinVsn} ->
		    case binary_to_list(BinVsn) of
			Vsn -> ok; %% already installed
			VsnNL -> ok; %% already installed
			_ -> install_(Home, Vsn)
		    end;
		{error,enoent} -> install_(Home,Vsn)
	    end
    end.

%% root should during real run be candy.AppDir
%% config is set to candy.AppDir/candy.config
install_(Home,Vsn) ->
    APPIMAGE = os:getenv("APPIMAGE", ""),
    APPDIR = os:getenv("APPDIR", ""),
    {User1,Admin1} = install_cmds_(Home,Vsn),
    Dir = filename:join(Home,?DOTAPP),
    file:make_dir(Dir),
    os:cmd(string:join(User1, ";")),
    log_commands(Dir,["# APPIMAGE", ["echo ",APPIMAGE]]),
    log_commands(Dir,["# APPDIR", ["echo ",APPDIR]]),
    log_commands(Dir,["# USER1"|User1]),
    if Admin1 =:= [] ->
	    ok;
       true ->
	    os:cmd("export SUDO_ASKPASS="++askpass()++"; sudo -A sh -c \""++
		       string:join(Admin1, ";")++"\""),
	    log_commands(Dir,["# ADMIN1"|Admin1]),
	    ok
    end.

install_cmds() ->
    Home = os:getenv("HOME"),
    {ok,Vsn} = application:get_key(?APP, vsn),
    install_cmds_(Home,Vsn).

install_cmds_(Home,Vsn) ->
    APPDIR = os:getenv("APPDIR", ""),
    APPIMG = os:getenv("APPIMAGE", ""),
    AppDir = case init:get_argument(root) of
		 {ok,[[APPDIR]]} ->
		     APPDIR;
		 _ ->
		     code:priv_dir(?APP)
	     end,
    {[lists:flatten(Cmd)||Cmd <- install_cmd_(Home, Vsn, AppDir,APPIMG)],
     []}.

install_cmd_(Home, Vsn, AppDir,AppImg) ->
    IconsDir = filename:join([AppDir,"desktop_icons","hicolor"]),
    case file:list_dir(IconsDir) of
	{ok,Sizes} ->
	    lists:append(
	      [
	       [
		["mkdir -p ", filename:join([Home,".local","share","icons","hicolor",Size,"apps"])],
		["cp ", 
		 filename:join([AppDir,"desktop_icons","hicolor",Size,
				"apps",?APPPNG])," ",
		 filename:join([Home,".local","share","icons","hicolor",Size,
				"apps",?APPPNG])]] || Size <- Sizes]);
	_ ->
	    [["echo icondir=",IconsDir]]
    end ++
    [
     ["cat ", filename:join(AppDir, ?APPDSK), 
      " | sed 's%APPIMAGE%",AppImg,"%' > ",
      filename:join([Home, "Desktop", string:titlecase(?APPDSK)])],
     ["mkdir -p ", filename:join(Home,?DOTAPP)],
     ["echo \"", Vsn, "\" > ", filename:join([Home,?DOTAPP,"installed"])]
    ].

%% log each command, one by one
log_commands(Dir, [Cmd|Cmds]) ->
    {ok,Fd} = file:open(filename:join(Dir,"install_log.txt"), [write,append]),
    io:put_chars(Fd, [Cmd,"\n"]),
    file:close(Fd),
    log_commands(Dir, Cmds);
log_commands(_Dir, []) ->
    ok.


start() ->
    application:load(?APP),
    install(),
    application:ensure_all_started(?APP),
    start_it([], start).

start_it(Options, Start) ->
    epxw:Start(?MODULE,
	       Options,
	       [{title, "Breakout"},
		{screen_color, ?GAME_BACKGROUND_COLOR},
		{top_bar, 0},
		{left_bar, 0},
		{right_bar, 0},
		{bottom_bar, 0},
		{width,?WINDOW_WIDTH},{height,?WINDOW_HEIGHT}
	       ]).

init(_Args) ->
    CX = ?WINDOW_WIDTH div 2,
    CY = ?WINDOW_HEIGHT div 2,
    Window = epxw:window(),
    epx:window_enable_events(Window, no_auto_repeat),
    epx:window_adjust(Window, [{min_width,?WINDOW_WIDTH},
			       {max_width,?WINDOW_WIDTH},
			       {min_height,?WINDOW_HEIGHT},
			       {max_height,?WINDOW_HEIGHT}]),
    erlang:start_timer(?TICK, self(), tick),
    catch xbus:sub("egear.slider.*"),
    Px = ?GAME_LEFT + (?GAME_WIDTH-?PADEL_WIDTH) div 2,
    Py = (?GAME_BOTTOM - 4*?BORDER),

    {ok, Plop} = read_sound("plop"),
    {ok, Peep} = read_sound("peep"),
    {ok, Beep={Options,_}} = read_sound("beep"),
    {ok, Alsa, AlsaOptions} = open_sound_dev(Options),

    {ok, 
     #game{
	nplayers = 1,
	player = 1,
	score1 = 0,
	score2 = 0,
	padel = #padel { box = {Px, Py, ?PADEL_WIDTH, ?PADEL_HEIGHT},
			 color = ?PADEL_COLOR },
	ball = ball_start(math:pi()/2, CX, CY, ?BALL_SPEED_0),
	brick_colors =
	    { {255,0,0}, {255,25,25}, 
	      {255,255,0}, {255,255,25}, 
	      {0,255,0}, {0,255,25}, 
	      {255,255,200}, {255,255,255} },
	bricks =
	    { 2#11111111111111,  %% red1
	      2#11111111111111,  %% red2
	      2#11111111111111,  %% yellow1
	      2#11111111111111,  %% yellow2
	      2#11111111111111,  %% green1
	      2#11111111111111,  %% green2
	      2#11111111111111,  %% white1
	      2#11111111111111   %% white2
	    },
	plop = Plop,
	peep = Peep,
	beep = Beep,
	alsa = Alsa,
	alsa_options = AlsaOptions
       }}.

%% game_new_ball(Game) ->
%%     Px = ?GAME_LEFT + ?PADEL_WALL_DIST, 
%%     %% Py = (?GAME_BOTTOM - 4*?BORDER),
%%     CX = ?WINDOW_WIDTH div 2,
%%     CY = ?WINDOW_HEIGHT div 2,
%%     P = Game#game.padel,
%%     A = math:fmod(3*math:pi()/2, math:pi()),
%%     Ball = ball_start(A, CX, CY, ?BALL_SPEED_0),
%%     Game#game { padel = P#padel { box = ?set_rect_x(P#padel.box, Px)},
%% 		ball = Ball }.

key_press({key_press, _Key=?PADEL_KEY_LEFT, _Mod, _Code}, Game) ->
    ?dbg("key_press ~p(1)\n", [[_Key]]),
    P = padel_press(Game#game.padel, -?PADEL_SPEED),
    epxw:invalidate(),
    Game#game { padel = P };
key_press({key_press, _Key=?PADEL_KEY_RIGHT, _Mod, _Code}, Game) ->
    ?dbg("key_press ~p(1)\n", [[_Key]]),
    P = padel_press(Game#game.padel, +?PADEL_SPEED),
    epxw:invalidate(),
    Game#game { padel = P };
key_press({key_press, _Key=$q, _Mod, _Code}, Game) ->
    ?dbg("key_press ~p\n", [[_Key]]),
    self() ! quit,
    Game;
key_press({key_press, _Key, _Mod, _Code}, Game) ->
    ?dbg("key_press ~p\n", [[_Key]]),
    Game.

key_release({key_release, _Key=?PADEL_KEY_LEFT, _Mod, _Code}, Game) ->
    P = padel_release(Game#game.padel),
    ?dbg("key_release ~p(1), pressed=~w, xdir=~w\n",
	 [[_Key], P#padel.pressed, P#padel.xdir]),
    Game#game { padel = P };
key_release({key_release, _Key=?PADEL_KEY_RIGHT, _Mod, _Code}, Game) ->
    P = padel_release(Game#game.padel),
    ?dbg("key_release ~p(1), pressed=~w, xdir=~w\n",
	 [[_Key], P#padel.pressed, P#padel.xdir]),
    Game#game { padel = P };
key_release({key_release, _Key, _Mod, _Code}, Game) ->
    ?dbg("key_release ~p\n", [[_Key]]),
    Game.

handle_info({timeout,_, tick}, Game) ->
    erlang:start_timer(?TICK, self(), tick),
    P = Game#game.padel,
    Box = P#padel.box,
    X1 = padel_step(?rect_x(Box), P#padel.xdir),
    {B, Game1} = ball_step(Game#game.ball, Game),
    epxw:invalidate(),
    {noreply, Game1#game { padel = P#padel { box = ?set_rect_x(Box,X1) },
			   ball = B }};

handle_info({xbus,_,#{ topic := <<"egear.slider.6a*.4.value">>, 
		       value := Value }}, Game) ->
    P = Game#game.padel,
    Box = P#padel.box,
    X = ?GAME_LEFT + (Value/255 * (?GAME_WIDTH-?PADEL_WIDTH)),
    {noreply, Game#game { padel = P#padel { box = ?set_rect_x(Box,X) }}};

handle_info(quit, Game) ->
    {stop, normal, Game};
handle_info(_Info, Game) ->
    ?dbg("got info ~p\n", [_Info]),
    {noreply, Game}.

padel_press(P, Step) ->
    Box = P#padel.box,
    X = padel_step(?rect_x(Box), Step),
    N = P#padel.pressed + 1,
    P#padel { pressed = N, box = ?set_rect_x(Box,X), xdir = Step }.

padel_release(P) ->
    case P#padel.pressed - 1 of
	0 -> P#padel { pressed = 0, xdir = 0 };
	N -> P#padel { pressed = N }
    end.

padel_step(Xpos, 0) -> Xpos;
padel_step(Xpos, Xdir) when Xdir < 0 ->
    max(?GAME_LEFT, Xpos + Xdir);
padel_step(Xpos, Xdir) when Xdir > 0 ->
    min(?GAME_RIGHT - ?PADEL_WIDTH, Xpos + Xdir).

ball_start(A, X, Y, Speed) ->
    %% calculate better! to avoid getting stuck
    #ball { pos = {X,Y},
	    vel = {math:cos(A),math:sin(A)},
	    speed = Speed }.

ball_step(B = #ball { pos = Pos0, vel = V }, Game) ->
    Pos1 = vec_add(Pos0, vec_scale(B#ball.speed, V)),
    %% ?dbg("step pos = ~w, vel = ~w\n", [Pos1, V]),
    case padel_bounce(Pos1, B#ball.radius, Game#game.padel) of
	false ->
	    case ball_brick(Pos1, Game#game.bricks) of
		false ->
		    case wall_bounce(Pos1) of
			false ->
			    {B#ball { pos = Pos1 }, Game};
			N ->
			    play_sound(Game#game.plop,Game),
			    {ball_bounce(B, Pos1, V, N), Game}
		    end;
		{true, Bricks, N} ->
		    play_sound(Game#game.peep,Game),
		    B1 = ball_bounce(B, Pos1, V, N),
		    Game1 = game_score(Game#game { bricks = Bricks }),
		    {B1, Game1}
	    end;
	N ->
	    play_sound(Game#game.plop,Game),
	    {ball_bounce(B, Pos1, V, N), Game}
    end.

ball_bounce(B, Pos1, V, N) ->
    %% normal
    U = vec_scale(vec_dot(V, N) / vec_len(N), N),
    W = vec_sub(V, U),
    V1 = vec_sub(vec_scale(?WALL_FRICTION, W),
		 vec_scale(?WALL_RESTITUTION, U)),
    Pos2 = vec_add(Pos1, vec_scale(B#ball.speed, V1)),
    %% ?dbg("bounce pos = ~w, vel = ~w\n", [Pos2, V1]),
    B#ball { pos = Pos2, vel = V1 }.

%% check if ball is touching brick 
ball_brick({X,Y}, Bricks) ->
    Y0 = 2*8*?DIGIT_DOT+2,
    I  = (trunc(Y - Y0) div ?BRICK_HEIGHT)+1,
    if I < 1 -> false;
       I > 8 -> false;
       true ->
	    J  = trunc(X) div ?BRICK_WIDTH,
	    %% J >= 0, J < ?BRICKS_PER_ROW?
	    BrickMask = element(I, Bricks),
	    if BrickMask band (1 bsl J) =:= 0 ->
		    false;
	       true ->
		    BrickMask1 = BrickMask band (bnot  (1 bsl J)),
		    Bricks1 = setelement(I, Bricks, BrickMask1),
		    {true, Bricks1, {0,-1}}
	    end
    end.


%% return the normal vector (and point) when ball hit padel
padel_bounce(Pos={X,Y}, R, Padel) ->
    %% if bounding boxes intersect
    case epx_rect:intersect({X-R,Y-R,2*R,2*R}, Padel#padel.box) of
	{0,0,0,0} -> false;
	_ ->
	    {Px,Py,Pw,Ph} = Padel#padel.box,
	    Ph2 = Ph div 2,	    
	    Ly = Py+Ph2,
	    A = {Px,Ly}, B={Px+Pw,Ly},
	    {Pl,Q} = vec_len2_to_line(Pos, A, B),
	    if Pl =< (Ph-1) ->
		    Ce = vec_add(A, vec_scale(1/2,vec_sub(B,A))),
		    {Ql,_} = vec_sub(Q, Ce),
		    Xd = -Ql / Pw,
		    vec_norm({Xd,1});
	       true ->
		    false
	    end
    end.

%% brick is down account
game_score(Game) ->
    case Game#game.player of
	1 ->
	    Score = Game#game.score1 + 1,
	    Game#game {score1 = Score };
	2 ->
	    Score = Game#game.score2 + 1,
	    Game#game {score2 = Score }
    end.

%% return normal vector of wall bounced into
wall_bounce({X,_Y}) when X =< ?GAME_LEFT  -> {1,0};
wall_bounce({X,_Y}) when X >= ?GAME_RIGHT -> {-1,0};
wall_bounce({_X,Y}) when Y =< ?GAME_TOP -> {0,1};
wall_bounce({_X,Y}) when Y >= ?GAME_BOTTOM -> {0,-1};
wall_bounce(_) -> false.

draw(Screen, _DirtyRect, Game) ->
    epx_gc:set_fill_style(solid),
    draw_game(Screen),
    draw_bricks(Screen, Game#game.bricks, Game#game.brick_colors),
    draw_padel(Screen, Game#game.padel),
    draw_score(Screen, 1, Game#game.score1, Game),
    if Game#game.nplayers > 1 ->
	    draw_score(Screen, 2, Game#game.score2, Game);
       true ->
	    ok
    end,
    draw_ball(Screen, Game#game.ball),
    Game.

draw_game(Screen) ->
    epx_gc:set_fill_color(?GAME_BORDER_COLOR),
    %% upper
    epx:draw_rectangle(Screen, 0, 0, ?WINDOW_WIDTH, ?BORDER),
    %% left
    epx:draw_rectangle(Screen, 0, ?BORDER, ?BORDER, ?WINDOW_HEIGHT-?BORDER),
    %% right
    epx:draw_rectangle(Screen, ?WINDOW_WIDTH-?BORDER,?BORDER,?BORDER,?WINDOW_HEIGHT-?BORDER),

    %% draw dotted lower 2*BORDER dots
    CY = ?WINDOW_HEIGHT- 2*?BORDER,
    N = ?GAME_WIDTH div (2*?BORDER),
    [begin
	 X = ?BORDER + I*2*?BORDER,
	 epx:draw_rectangle(Screen, X, CY, 2*?BORDER, ?BORDER)
     end || I <- lists:seq(0, N, 2)],
    ok.

draw_bricks(Screen, Bricks, Colors) ->
    Y0 = 2*8*?DIGIT_DOT+2,
    draw_bricks_(Screen, Y0, 1, Bricks, Colors).

draw_bricks_(_Screen, _Y, I, _, _) when I > 8 ->
    ok;
draw_bricks_(Screen, Y, I, Bricks, Colors) ->
    Mask = element(I, Bricks),
    Color = element(I, Colors),
    epx_gc:set_fill_color(Color),
    draw_brick_row(Screen, Y, Mask),
    draw_bricks_(Screen, Y+?BRICK_HEIGHT, I+1, Bricks, Colors),
    ok.

draw_brick_row(Screen, Y, Mask) ->
    H = ?BRICK_HEIGHT,
    W = ?BRICK_WIDTH,
    X0 = 0, %% ?BORDER,
    draw_brick_row_(Screen, X0, Y, W, H, 0, Mask).

draw_brick_row_(_Screen, X, Y, _W, _H, I, _Mask) when I >= ?BRICKS_PER_ROW ->
    {X, Y};
draw_brick_row_(Screen, X, Y, W, H, I, Mask) -> 
    if Mask band (1 bsl I) =/= 0 ->
	    epx:draw_rectangle(Screen, X, Y, W-2, H-2),
	    draw_brick_row_(Screen, X+W, Y, W, H, I+1, Mask);
       true ->
	    draw_brick_row_(Screen, X+W, Y, W, H, I+1, Mask)
    end.
    

draw_padel(Screen, #padel{box=Rect,color=Color}) ->
    epx_gc:set_fill_color(Color),
    epx:draw_rectangle(Screen, Rect).

draw_ball(Screen, #ball{pos={X,Y},radius=R,color=Color}) ->
    epx_gc:set_fill_color(Color),
    epx:draw_ellipse(Screen, X-R,Y-R,2*R,2*R).

draw_score(Screen, 1, Score, _Game) ->
    epx_gc:set_fill_color(?SCORE_COLOR),
    X0 = 2*?BORDER,
    Y0 = ?BORDER + 1,
    draw_digits(Screen, X0, Y0, "1"),
    Text = integer_to_list(Score),
    X = X0 + 5*?DIGIT_DOT,
    Y = Y0 + 7*?DIGIT_DOT,
    draw_digits(Screen, X, Y, Text);

draw_score(Screen, 2, Score, _Game) ->
    epx_gc:set_fill_color(?SCORE_COLOR),
    X0 = (?WINDOW_WIDTH div 2) + 2*?BORDER,
    Y0 = ?BORDER + 1,
    draw_digits(Screen, X0, Y0, "2"),
    Text = integer_to_list(Score),
    X = X0 + 5*?DIGIT_DOT,
    Y = Y0 + 7*?DIGIT_DOT,
    draw_digits(Screen, X, Y, Text).

read_sound(Name) ->
    File = filename:join(code:priv_dir(breakout), Name ++ ".wav"),
    case file:open(File, [read, binary]) of
	{ok, Fd} ->
	    case alsa_wav:read_header(Fd) of
		{ok, Header} ->
		    {ok, Cur} = file:position(Fd, cur),
		    {ok, End} = file:position(Fd, eof),
		    Size = (End - Cur)+1,
		    {ok, _} = file:position(Fd, Cur),
		    {ok, Data} = file:read(Fd, Size),
		    file:close(Fd),
		    {ok,{Header,Data}}
	    end;
	Error ->
	    Error
    end.

%% return {ok, Handle, Options} | {error, Reason}
open_sound_dev(Options0) ->
    Options = maps:merge(Options0, #{ latency =>  20}),
    alsa_playback:open(Options).


play_sound({_Header,Samples}, 
	   #game { alsa = Handle, alsa_options = Options } ) ->
    #{ format := Format,
       channels := Channels,
       period_size := PeriodSize } = Options,
    Silence = alsa:make_silence(Format, Channels, PeriodSize),
    spawn(
      fun() ->
	      alsa:prepare_(Handle),
	      alsa:write(Handle, Silence),
	      alsa:write(Handle, Samples)
      end).

%% vector
vec_add({X1,Y1}, {X2,Y2}) -> {X1+X2,Y1+Y2}.
vec_sub({X1,Y1}, {X2,Y2}) -> {X1-X2,Y1-Y2}.
vec_scale(F, {X1,Y1}) -> {F*X1, F*Y1}.
vec_dot({X1,Y1}, {X2,Y2}) -> X1*X2 + Y1*Y2.
vec_len2(V) -> vec_dot(V, V).
vec_len2(V,W) -> vec_len2(vec_sub(W,V)).
vec_len(V) -> math:sqrt(vec_len2(V)).
vec_norm(V) -> vec_scale(1/vec_len(V), V).

vec_len2_to_line(P, V, W) ->
    case vec_len2(V, W) of
	0 ->
	    {vec_len2(P, V), V};
	L2 ->
	    PV = vec_sub(P, V),
	    WV = vec_sub(W, V),
	    T = max(0, min(1.0, vec_dot(PV, WV)/L2)),
	    Q = vec_add(V, vec_scale(T, WV)),
	    {vec_len2(P, Q), Q}
    end.

%% internal "font"

draw_digits(Screen, X, Y, Ds) ->
    draw_digits_(Screen, X, Y, Ds).

draw_digits_(_Screen, X, Y, []) ->
    {X,Y};
draw_digits_(Screen, X, Y, [D|Ds]) ->
    {X1,_} = draw_digit(Screen, X, Y, D),
    draw_digits_(Screen, X1+?DIGIT_DOT, Y, Ds).

draw_digit(Screen, X, Y, D) ->
    draw_digit_(Screen, X, Y, 0, pong_glyph(D-$0)).

draw_digit_(_Screen, _X, Y, X1, []) ->
    {X1,Y};
draw_digit_(Screen, X, Y, _X1, [Row|Rs]) ->
    X1 = draw_row(Screen, X, Y, Row),
    draw_digit_(Screen, X, Y+?DIGIT_DOT, X1, Rs).

draw_row(Screen, X, Y, [1|Row]) ->
    epx:draw_rectangle(Screen, X, Y, ?DIGIT_DOT, ?DIGIT_DOT),
    draw_row(Screen, X+?DIGIT_DOT, Y, Row);
draw_row(Screen, X, Y, [0|Row]) ->
    draw_row(Screen, X+?DIGIT_DOT, Y, Row);
draw_row(_Screen, X, _Y, []) ->
    X.


pong_glyph(0) ->
    [[1,1,1,1,1],
     [1,0,0,0,1],
     [1,0,0,0,1],
     [1,0,0,0,1],
     [1,0,0,0,1],
     [1,0,0,0,1],
     [1,1,1,1,1]];
pong_glyph(1) ->
    [[0,0,1,0,0],
     [0,0,1,0,0],
     [0,0,1,0,0],
     [0,0,1,0,0],
     [0,0,1,0,0],
     [0,0,1,0,0],
     [0,0,1,0,0]];
pong_glyph(2) ->
    [[1,1,1,1,1],
     [0,0,0,0,1],
     [0,0,0,0,1],
     [1,1,1,1,1],
     [1,0,0,0,0],
     [1,0,0,0,0],
     [1,1,1,1,1]];
pong_glyph(3) ->
    [[1,1,1,1,1],
     [0,0,0,0,1],
     [0,0,0,0,1],
     [1,1,1,1,1],
     [0,0,0,0,1],
     [0,0,0,0,1],
     [1,1,1,1,1]];
pong_glyph(4) ->
    [[1,0,0,0,1],
     [1,0,0,0,1],
     [1,0,0,0,1],
     [1,1,1,1,1],
     [0,0,0,0,1],
     [0,0,0,0,1],
     [0,0,0,0,1]];
pong_glyph(5) ->
    [[1,1,1,1,1],
     [1,0,0,0,0],
     [1,0,0,0,0],
     [1,1,1,1,1],
     [0,0,0,0,1],
     [0,0,0,0,1],
     [1,1,1,1,1]];
pong_glyph(6) ->
    [[1,1,1,1,1],
     [1,0,0,0,0],
     [1,0,0,0,0],
     [1,1,1,1,1],
     [1,0,0,0,1],
     [1,0,0,0,1],
     [1,1,1,1,1]];
pong_glyph(7) ->
    [[1,1,1,1,1],
     [0,0,0,0,1],
     [0,0,0,0,1],
     [0,0,0,0,1],
     [0,0,0,0,1],
     [0,0,0,0,1],
     [0,0,0,0,1]];
pong_glyph(8) ->
    [[1,1,1,1,1],
     [1,0,0,0,1],
     [1,0,0,0,1],
     [1,1,1,1,1],
     [1,0,0,0,1],
     [1,0,0,0,1],
     [1,1,1,1,1]];
pong_glyph(9) ->
    [[1,1,1,1,1],
     [1,0,0,0,1],
     [1,0,0,0,1],
     [1,1,1,1,1],
     [0,0,0,0,1],
     [0,0,0,0,1],
     [0,0,0,0,1]].
