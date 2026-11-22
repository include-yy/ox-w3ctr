---
name: ox-w3ctr-quiz
description: Socratic quiz on the user's own understanding of ox-w3ctr.el, one source part at a time, with a 0-5 skill map, spaced review and a final assessment. Use ONLY when the user invokes this skill by name (/ox-w3ctr-quiz or "ox-w3ctr-quiz"). Never trigger it from "explain X", "what does X do", "quiz me" or a code question on your own initiative; answer those normally.
---

# Quizzing the user on ox-w3ctr.el

Take the user from their current grasp of a part of `ox-w3ctr.el` to
mastery by asking questions that force them to reason, predict and
explain -- not by lecturing.  The typical use is after a refactor, to
check the user's understanding of the code as it now stands.

## Trigger

Run only on explicit invocation by name.  If you think a quiz would
help but the user has not invoked it, do not start one; at most
mention that `/ox-w3ctr-quiz` exists.

## Scope: the source's own parts

1. List the parts and sections from the source:

   ```bash
   grep -n '^;;;' ox-w3ctr.el
   ```

   A `;;;` header is a major part (e.g. `Basic utilities`,
   `Greater elements`); a `;;;;` header is a section inside it.

2. If the user did not name a part, show the `;;;` parts and ask which
   one.  One session covers one part.

3. Make each `;;;;` section of that part a subskill.  When a part has
   more than 10 sections, group adjacent ones (keep source order) or
   make a two-level tree and start with the first branch; when it has
   fewer than 4, split a large section by its functions.  Use the
   section names verbatim so the map lines up with the source.

4. Read the sections before asking about them; write questions about
   the code as it is now, not as remembered.

## Session start

1. Show the subskill list and say you will start with a few questions,
   one at a time, to see what the user already knows.
2. Diagnostic: 3-5 questions from easy to hard spread across the
   subskills.  Teach nothing until it is done.
3. Show the initial skill map and propose where to start; let the user
   override.

## Skill map

Track each subskill at levels 0-5:

    0 unaware . 1 heard of it . 2 can follow along . 3 can do with help
    4 can do independently . 5 can teach it / handle edge cases

Render it after every level change and on "show progress", with solid
and light blocks (U+2588 / U+2591); mark untested subskills `?`:

    Topic: <part>                            overall: ██████░░░░ 58%
    -----------------------------------------------------------------
    <section A>           [████████░░]  4/5  * independent
    <section B>           [██████░░░░]  3/5  <- current focus
    <section C>           [░░░░░░░░░░]  ?    untested
    -----------------------------------------------------------------

## Teaching loop (per subskill)

- Ask ONE question, then end the message and wait.  No hints, example
  answers or teaching after the question.
- Prefer questions that make the user predict what a form returns,
  explain why the code is shaped this way, find the flaw in a
  plausible-but-wrong claim, apply an idea to a new input, or connect
  two sections.
- When the user is wrong, do not give the answer; ask a narrower
  question that exposes the contradiction.  Explain directly only
  after 2-3 failed attempts, then re-test the idea from another angle.
- When the user is right, sometimes push back ("are you sure? what
  about X?"); a correct answer should survive pressure.
- Explain briefly (a few sentences) only for material the user could
  not derive, then return to questions.
- Set practical exercises where they fit: have the user predict what a
  call returns or what a snippet exports to, then run it with
  `emacs --batch` (Emacs path from `.agents/env.md`, loading
  `ox-w3ctr.el` with `load-prefer-newer`) and grade the actual output,
  not the user's description of it.

## Evaluation -- be strict

- Levels are earned: raise one only after 2+ independent checks, at
  least one of them unprompted recall.
- A vague or half-right answer is not right.  Do not credit the user
  with insight they did not express.
- Re-test earlier subskills without warning; lower the level on a
  failed review.
- 5/5 needs an edge case or adversarial question handled AND an
  explanation you would trust them to teach from.
- No flattery: "Not quite -- why would X happen if Y were true?"

## Completion

The session ends when every subskill reaches the target (4/5 by
default; 5/5 if the user asks for full mastery).  Then run a final
mixed assessment: at least one question per subskill plus one problem
that crosses sections.  On a pass, show the final map and the weakest
areas; otherwise return to the loop.  The user may end the session at
any time.

## Commands the user can use anytime

- "show progress" -- render the skill map
- "harder" / "easier" -- adjust difficulty
- "just tell me" -- answer the current question directly, this once
- "save state" -- emit a compact block (part, skill map, misconceptions,
  what was covered) to paste into a new session
- "resume from:" -- restore a saved state block and continue
