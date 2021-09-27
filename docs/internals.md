

# flagman


## Overall

**flagman** is a long-living, supervised process which only monitors the system [runq](#org9371d4a) and ensures [actions](#org8c09729) are taken when conditions are met.

By design, the process runs as high priority and avoids communicating with other processes.


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
<td class="org-left">Runq<a id="org9371d4a"></a></td>
<td class="org-left">The length of beam system run_queue, checked periodly, see <a href="#org4cb94ed">T1</a> and <a href="#orgb73e26d">T2</a>.</td>
</tr>


<tr>
<td class="org-left">Scheduler<a id="org27b202f"></a></td>
<td class="org-left">Number of <b>Online</b> schedulers.</td>
</tr>


<tr>
<td class="org-left">Credits<a id="org1a5f54f"></a></td>
<td class="org-left">Gain credits when <a href="#org9371d4a">runq</a> is under limit, lose credits when <a href="#org9371d4a">runq</a> is over limit.</td>
</tr>


<tr>
<td class="org-left">Priority Group<a id="orgce9e1a6"></a></td>
<td class="org-left">Process could join/leave prioriy groups. Load contol takes different actions when conditions are meet.</td>
</tr>


<tr>
<td class="org-left">Overloaded<a id="orgd831e5b"></a></td>
<td class="org-left">State when <a href="#org1a5f54f">credits</a> runs out. System is overloaded and action must be taken.</td>
</tr>


<tr>
<td class="org-left">Warm<a id="org52c1aff"></a></td>
<td class="org-left">State when system is losing/gaining <a href="#org1a5f54f">credits</a>, a middle state preventing system is shifting between <a href="#org9224d40">cold</a> and <a href="#orgd831e5b">overloaded</a></td>
</tr>


<tr>
<td class="org-left">Cold<a id="org9224d40"></a></td>
<td class="org-left">State when system is full of <a href="#org1a5f54f">credits</a></td>
</tr>


<tr>
<td class="org-left">Flag<a id="org5c353c7"></a></td>
<td class="org-left">Regisered process name when system is <a href="#orgd831e5b">overloaded</a></td>
</tr>


<tr>
<td class="org-left">Actions<a id="org8c09729"></a></td>
<td class="org-left">Actions to take to cool down the system. Action is taken by either flagman or API caller</td>
</tr>
</tbody>
</table>


## [Actions](#org8c09729) to take:


### Control the [flag](#org5c353c7)

-   ensure the [flag](#org5c353c7) is rasied when system is <a id="org37876ff"></a>.
-   ensure the [flag](#org5c353c7) is cleared when system is cold


### Brutal kill processes in low (<= [F3](#org636b2c7)) [priority group](#orgce9e1a6) in either of any conditions listed below

1.  [flag](#org5c353c7) is rasied
2.  [credits](#org1a5f54f) left percentage blow [F4](#orgafc3c45)


### Caller get scheduled out for a limited amount of time if the system is [overloaded](#orgd831e5b)

see `load_ctl:maydelay`

Caller is back on scheduler when [flag](#org5c353c7) is cleared.

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
<td class="org-left">F0<a id="orgb1a9b63"></a></td>
<td class="org-left">boolean()</td>
<td class="org-right">true</td>
<td class="org-left">Set it to false to disable the load control function. To reenable, you should call <code>load_ctl:restart_runq_flagman/0</code>.</td>
</tr>


<tr>
<td class="org-left">F1<a id="org7e972bc"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">8</td>
<td class="org-left"><a href="#org27b202f">scheduler</a> multipler, <a href="#org9371d4a">runq</a> is overlimit when <a href="#org9371d4a">runq</a> &gt; <a href="#org27b202f">scheduler</a> * <a href="#org7e972bc">F1</a></td>
</tr>


<tr>
<td class="org-left">F2<a id="org2f06081"></a></td>
<td class="org-left">float()</td>
<td class="org-right">0.8</td>
<td class="org-left"><a href="#org27b202f">scheduler</a> utilization limit for increasing <a href="#org1a5f54f">credits</a></td>
</tr>


<tr>
<td class="org-left">F3<a id="org636b2c7"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">2</td>
<td class="org-left">Priority threshold for killing processes in <a href="#orgce9e1a6">priority group</a> forcefully.</td>
</tr>


<tr>
<td class="org-left">F4<a id="orgafc3c45"></a></td>
<td class="org-left">integer() &lt; 100</td>
<td class="org-right">50</td>
<td class="org-left"><a href="#org1a5f54f">credits</a> left precentage.</td>
</tr>


<tr>
<td class="org-left">T1<a id="org4cb94ed"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">3000 (ms)</td>
<td class="org-left">Regular <a href="#org9371d4a">runq</a> check interval, see also <a href="#orgb73e26d">T2</a></td>
</tr>


<tr>
<td class="org-left">T2<a id="orgb73e26d"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">1000 (ms)</td>
<td class="org-left"><a href="#org9371d4a">runq</a> check interval when it is overlimit. See <a href="#org7e972bc">F1</a></td>
</tr>


<tr>
<td class="org-left">C2<a id="org352505e"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">3</td>
<td class="org-left">Cooldown <a href="#org1a5f54f">credits</a>.</td>
</tr>
</tbody>
</table>


## state machine

![img](flagman_fsm.png)


# KILL [Dropped Idea] runq monitor

The main reason I drop this idea is that there seems no any benefit to make it a gen\_statm
because it should not handle any calls from other processes to minimize any undesired workload and let it be standalone.


## API


### active check system is overloaded

fun is\_overload()-> boolean()


### passive check


## state machine

![img](state_machine.png)

