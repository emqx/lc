

# Runq Flagman


## Overall

**runq flagman** is a long-living, supervised process which only monitors the system [runq](#org9c90866) and ensures [actions](#orga69ea5c) are taken when conditions are met.

By design, the process runs as high priority and avoids communicating with other processes.

**runq flagman** can also take its own scheduling roundtrip time into a factor ([F5](#org9478e54)) to speed up the detection of system overload.
This is done by taking timestamps when it is off and on the scheduler to check if there is a time leap. If the gap is large enough
that is a sign that VM has very limited resources on scheduling.


## Terminology

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Term</th>
<th scope="col" class="org-left">Definition</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Runq<a id="org9c90866"></a></td>
<td class="org-left">The length of beam system run_queue, checked periodly, see <a href="#org0c878de">T1</a> and <a href="#orgc4a96e5">T2</a>.</td>
</tr>


<tr>
<td class="org-left">Scheduler<a id="orgdc471ce"></a></td>
<td class="org-left">Number of <b>Online</b> schedulers.</td>
</tr>


<tr>
<td class="org-left">Credits<a id="orga7c5eb7"></a></td>
<td class="org-left">Gain credits when <a href="#org9c90866">runq</a> is under limit, lose credits when <a href="#org9c90866">runq</a> is over limit.</td>
</tr>


<tr>
<td class="org-left">Priority Group<a id="orge4d75da"></a></td>
<td class="org-left">Process could join/leave prioriy groups. Load contol takes different actions when conditions are meet.</td>
</tr>


<tr>
<td class="org-left">Overloaded<a id="orgf0e1dc9"></a></td>
<td class="org-left">State when <a href="#orga7c5eb7">credits</a> runs out. System is overloaded and action must be taken.</td>
</tr>


<tr>
<td class="org-left">Warm<a id="org0a32ca3"></a></td>
<td class="org-left">State when system is losing/gaining <a href="#orga7c5eb7">credits</a>, a middle state preventing system is shifting between <a href="#orge35209e">cold</a> and <a href="#orgf0e1dc9">overloaded</a></td>
</tr>


<tr>
<td class="org-left">Cold<a id="orge35209e"></a></td>
<td class="org-left">State when system is full of <a href="#orga7c5eb7">credits</a></td>
</tr>


<tr>
<td class="org-left">Flag<a id="org9f428f5"></a></td>
<td class="org-left">Regisered process name when system is <a href="#orgf0e1dc9">overloaded</a></td>
</tr>


<tr>
<td class="org-left">Actions<a id="orga69ea5c"></a></td>
<td class="org-left">Actions to take to cool down the system. Action is taken by either flagman or API caller</td>
</tr>
</tbody>
</table>


## [Actions](#orga69ea5c) to take:


### Control the [flag](#org9f428f5)

-   ensure the [flag](#org9f428f5) is rasied when system is <a id="orgb4ee8d9"></a>.
-   ensure the [flag](#org9f428f5) is cleared when system is cold


### Brutal kill processes in low (<= [F3](#orgb448bed)) [priority group](#orge4d75da) in either of any conditions listed below

1.  [flag](#org9f428f5) is rasied
2.  [credits](#orga7c5eb7) left percentage blow [F4](#org95daa30)


### Caller get scheduled out for a limited amount of time if the system is [overloaded](#orgf0e1dc9)

see `load_ctl:maydelay`

Caller is back on scheduler when [flag](#org9f428f5) is cleared.

**TODO** Caller could be scheduled out when the system is **warm** as well?


## config parameters

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">parm name</th>
<th scope="col" class="org-left">type</th>
<th scope="col" class="org-right">default</th>
<th scope="col" class="org-left">comments</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">F0<a id="org6aaa6d7"></a></td>
<td class="org-left">boolean()</td>
<td class="org-right">true</td>
<td class="org-left">Set it to false to disable the load control function. To reenable, you should call <code>load_ctl:restart_runq_flagman/0</code>.</td>
</tr>


<tr>
<td class="org-left">F1<a id="org7cc736a"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">8</td>
<td class="org-left"><a href="#orgdc471ce">scheduler</a> multipler, <a href="#org9c90866">runq</a> is overlimit when <a href="#org9c90866">runq</a> &gt; <a href="#orgdc471ce">scheduler</a> * <a href="#org7cc736a">F1</a></td>
</tr>


<tr>
<td class="org-left">F2<a id="org6cadf4f"></a></td>
<td class="org-left">float()</td>
<td class="org-right">0.8</td>
<td class="org-left"><a href="#orgdc471ce">scheduler</a> utilization limit for increasing <a href="#orga7c5eb7">credits</a></td>
</tr>


<tr>
<td class="org-left">F3<a id="orgb448bed"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">2</td>
<td class="org-left">Priority threshold for killing processes in <a href="#orge4d75da">priority group</a> forcefully.</td>
</tr>


<tr>
<td class="org-left">F4<a id="org95daa30"></a></td>
<td class="org-left">integer() &lt; 100</td>
<td class="org-right">50</td>
<td class="org-left"><a href="#orga7c5eb7">credits</a> left percentage.</td>
</tr>


<tr>
<td class="org-left">F5<a id="org9478e54"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">0</td>
<td class="org-left">Scheduler turnaround time threshold (in ms) for extra <a href="#orga7c5eb7">credits</a> loss. 0 is off</td>
</tr>


<tr>
<td class="org-left">T1<a id="org0c878de"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">3000 (ms)</td>
<td class="org-left">Regular <a href="#org9c90866">runq</a> check interval, see also <a href="#orgc4a96e5">T2</a></td>
</tr>


<tr>
<td class="org-left">T2<a id="orgc4a96e5"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">1000 (ms)</td>
<td class="org-left"><a href="#org9c90866">runq</a> check interval when it is overlimit. See <a href="#org7cc736a">F1</a></td>
</tr>


<tr>
<td class="org-left">C2<a id="org216b72d"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">3</td>
<td class="org-left">Cooldown <a href="#orga7c5eb7">credits</a>.</td>
</tr>
</tbody>
</table>


## state machine

![img](flagman_fsm.png)


# Memory Flagman

Similar to **Runq Flagman**, **Memory Flagman** monitors the system memory usage every [T1](#orgc51da2d) from

-   /proc/meminfo
-   *sys/fs/cgroup*

Once it is over threshold [F1](#orgc827429), it rasies the flag and alarm.


## config parameters

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">parm name</th>
<th scope="col" class="org-left">type</th>
<th scope="col" class="org-right">default</th>
<th scope="col" class="org-left">comments</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">F0<a id="orgf4f20c0"></a></td>
<td class="org-left">boolean()</td>
<td class="org-right">true</td>
<td class="org-left">Set it to false to disable the flagman. To reenable, you should call <code>load_ctl:restart_mem_flagman/0</code>.</td>
</tr>


<tr>
<td class="org-left">F1<a id="orgc827429"></a></td>
<td class="org-left">float()</td>
<td class="org-right">0.75</td>
<td class="org-left">memory threshold when flag should be raised</td>
</tr>


<tr>
<td class="org-left">T1<a id="orgc51da2d"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">1000</td>
<td class="org-left">Check interval in ms</td>
</tr>
</tbody>
</table>


# KILL [Dropped Idea] runq monitor

The main reason I drop this idea is that there seems no any benefit to make it a gen\_statm
because it should not handle any calls from other processes to minimize any undesired workload and let it be standalone.


## API


### active check system is overloaded

fun is\_overload()-> boolean()


### passive check


## state machine

![img](state_machine.png)

