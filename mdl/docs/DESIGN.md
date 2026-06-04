# Проектные решения MDL v3

Этот документ фиксирует ключевые проектные решения языка `mdl` и
сопутствующего инструментария. Он написан как исходный материал для научной
статьи: акцент сделан не только на синтаксисе, но и на мотивации, семантических
границах, проверяемости и связи между языком, транслятором, рантаймом,
выравниванием моделей и решателем DDL-LTLf.

## 1. Назначение языка

`mdl` проектируется как компактный ML-подобный язык моделирования
нормативных положений. Его основная роль - промежуточное представление между
текстовыми или извлеченными из текста спецификациями и формальными проверками
на конечных трассах.

Принятые решения:

1. Язык не является универсальным языком программирования. Он ограничен
   моделями, чистыми вычислениями, темпоральными формулами, деонтическими
   правилами, фактами и межмодельными выравниваниями.
2. Язык разделяет три уровня:
   - термовый уровень: чистые значения, типы, функции, записи, суммы,
     сопоставление с образцом;
   - темпоральный уровень: формулы LTLf над конечной трассой;
   - деонтический уровень: правила обязательства, разрешения и запрета с
     приоритетами и исключениями.
3. Язык ориентирован на трассируемость. Правила именуются, сохраняют позиции в
   исходном тексте, поддерживают аннотации, а решатель возвращает unsat-core с
   привязкой к правилам, фактам и строкам.
4. Язык проектируется как "Python-first" и "text-first" одновременно:
   канонический текстовый синтаксис существует наряду с публичным Python AST и
   `ModelBuilder`, чтобы модель можно было получить как из редактора, так и из
   внешнего пайплайна извлечения знаний.

Следствие для статьи: `mdl` следует описывать как предметно-ориентированный
язык для нормативного моделирования и проверки, а не как новую реализацию
общего функционального языка.

## 2. Архитектурное решение

Архитектура намеренно разложена на слои, каждый из которых соответствует
отдельному уровню семантики:

```text
source .mdl
  -> lexer/parser
  -> public Python AST dataclasses
  -> formatter / linter / runtime / aligner
  -> DDL-LTLf core JSON
  -> bounded Z3 solver
```

Ключевые решения:

1. Авторитетный парсер находится в `src/mdl/parser.py`. Грамматики VS Code и
   Tree-sitter являются редакторскими артефактами и могут быть более
   разрешающими, чем фактический язык.
2. AST является публичным API. Все основные узлы представлены dataclass-ами в
   `mdl.ast`, а не скрытой внутренней структурой.
3. Форматтер печатает каноническую текстовую форму. Это позволяет использовать
   цикл "Python AST -> MDL source -> parser" как нормальный путь построения
   моделей.
4. Транслятор в core JSON не привязан к Z3. Он сохраняет DDL-LTLf-подобную
   структуру, но оставляет атомы и функции достаточно абстрактными для других
   backend-ов.
5. Z3-решатель - один из backend-ов, а не полная семантика языка. Он реализует
   bounded-проверку на конечных горизонтах.
6. CLI имеет один исполняемый entry point `mdl` с подкомандами `parse`,
   `format`, `lint`, `translate`, `run`, `align`, `solve`, `lsp`.

## 3. Лексический и синтаксический стиль

### 3.1. ML-подобный синтаксис

Язык использует компактную ML-подобную форму:

```mdl
module pipe_spec

type Pipe = { length: rat, radius: rat }

entity pipe: Pipe

rule O pipe_length_positive: pipe.length > 0 always

fact pipe = Pipe { length = 10, radius = 2 }
```

Решения:

1. Модуль начинается с обязательного `module name`.
2. Верхнеуровневые декларации идут после модуля и импортов.
3. Имена могут быть квалифицированными через точку: `m1.pipe.length`.
4. Идентификаторы допускают буквы, цифры, `_` и `'`, но не могут начинаться с
   цифры.
5. Большая буква в начале локального имени используется как конвенция для
   конструкторов ADT.

### 3.2. Отступы и блоки

Функциональные тела используют отступы:

```mdl
func pwr(a: int, n: int) -> int:
    if n = 0 then 1
    else a * pwr(a, n - 1)
```

Решения:

1. Lexer генерирует `INDENT` и `DEDENT`, когда отступ меняется вне скобок.
2. Внутри круглых, квадратных и фигурных скобок переносы строк не создают
   структурные отступы.
3. Табуляция считается шириной 4.
4. Блок функции состоит из нуля или более `let`-операторов и финального
   выражения.
5. Для коротких тел поддерживается однострочная форма после `:`.

### 3.3. Комментарии и аннотации

Решения:

1. Поддержаны line-комментарии `#`
2. Строка, начинающаяся с `@`, является аннотацией следующего модуля,
   импорта или декларации.
3. Аннотация хранится как строка, без внутренней типизации. Это сохраняет
   свободу для ссылок на статьи закона, RFC, URI, разделы документов и внешние
   идентификаторы.
4. Сгенерированные alignment-модули используют комментарии вида
   `# align kind=... score=...`, а не `@ align ...`. Это делает provenance
   видимым в тексте, но не вводит дополнительную семантическую аннотацию.

### 3.4. Намеренные синтаксические отказы

В v3 зафиксированы несколько отрицательных решений:

1. Старый синтаксис правил через `=` отвергнут. Правило пишется через `:`.

   ```mdl
   rule O r: x always
   ```

   Синтаксис `rule O r = x always` считается ошибкой парсинга.

2. Неформальные braced expressions отвергнуты. Фигурные скобки у выражения
   зарезервированы для именованных record-конструкторов:

   ```mdl
   Pipe { length = 10, radius = 2 }
   ```

   Голая форма `{ x }` не является выражением.

3. Текстовый язык v3 не делает литералы коллекций каноническим синтаксисом.
   Коллекции выражаются через явно импортированные ADT из stdlib, например
   `List.Cons(1, List.Empty(()))`. Это исключает неявную магию вокруг `[]` и `#{}`.

4. Record value всегда строится через имя типа, а не через bare record:

   ```mdl
   Complex { r = a.r + b.r, i = a.i + b.i }
   ```

   Это решение делает конструктор номинальным, даже когда поля записи
   структурны.

## 4. Модули, импорты и видимость

### 4.1. Модули

Модуль задает пространство имен:

```mdl
module std.collections.list
```

Решения:

1. Имя модуля является квалифицированным именем.
2. Модуль является основной единицей парсинга, форматирования, lint-проверки и
   solve-проверки.
3. В solver-е имя модуля включается в полные имена правил и сущностей:
   `pipe_spec.pipe`, `pipe_spec.pipe_length_positive`.

### 4.2. Импорты

Поддерживаются файловые и модульные импорты:

```mdl
import "std/collections/list.mdl" as List exposing (List, len)
import "./pipe.mdl" as m1
```

Решения:

1. Импорт может иметь alias через `as`.
2. Импорт может явно открыть имена через `exposing (...)`.
3. В `exposing` поддержано переименование: `exposing (Pipe as PipeModel)`.
4. Stdlib импортируется явно через путь `std/...`; коллекции не являются
   глобальными builtins.
5. `ImportResolver` ищет импорт в открытых LSP-документах, stdlib и файловой
   системе.
6. Если импорт не найден, linter сообщает `unresolved-import`, а solver
   сообщает ошибку, если требуемый импорт отсутствует среди solve-входов или не
   может быть собран.

### 4.3. Public/private

Решения:

1. Декларации по умолчанию `public`.
2. `private` используется для реализации внутри модуля, например:

   ```mdl
   private type ProcessingState = LocalPart(unit) | Domain(unit)
   private func email_is_correct(email: string) -> bool:
       ...
   ```

3. Linter при построении импортируемой поверхности учитывает публичные
   декларации как видимые через alias.
4. Видимость является языковым решением об API модуля, а не полноценной
   security-моделью.

## 5. Типы

### 5.1. Примитивы

Примитивные типы:

```text
bool, int, rat, decimal, string, unit
```

Решения:

1. `bool` используется для формул и предикатов.
2. `int` кодируется в Z3 как `Int`.
3. `rat`, `rational`, `real`, `decimal` кодируются solver-ом как `Real`.
4. `string` кодируется как Z3 String.
5. `unit` представляет отсутствие значимого значения; единственное значение
   этого типа записывается как `()`.

### 5.2. TypeRef, record, tuple

Типовые выражения включают:

```mdl
Pipe
List<int>
{ length: rat, radius: rat }
(int, string)
```

Решения:

1. `TypeRef` поддерживает параметры типа через `<...>`.
2. Record type задается структурно как набор именованных полей.
3. Tuple type задается позиционно.
4. Поля tuple в tooling и solver-е получают имена `_0`, `_1`, ...
5. Record constructor остается номинальным: `Pipe { ... }`.
6. Record constructor проверяется на пропущенные, неизвестные и дублирующиеся
   поля.

### 5.3. Sum types и ADT

Суммы задаются через варианты:

```mdl
type ProcessingState = LocalPart(unit) | Domain(unit)
type Nat = Zero(unit) | Succ(Nat)
type Pair = Pair(int, int)
```

Решения:

1. Sum type является основной формой ADT.
2. Вариант может иметь позиционные поля: `Succ(Nat)`.
3. Вариант может иметь именованные поля: `Variant(name: Type)`.
4. Тип с одним конструктором и полями трактуется как одно-конструкторная сумма.
5. Solver кодирует суммы через Z3 datatypes.
6. Рекурсивные ADT поддерживаются, например `Nat = Zero(unit) | Succ(Nat)`.
7. Нульарные варианты не являются каноническим синтаксисом. Для marker-like
   cases используется payload `unit`, например `Local(())`.
8. Type aliases не поддерживаются: `type T = Existing` является ошибкой
   парсинга.

### 5.4. Параметрический полиморфизм

Решения:

1. Типы и функции могут иметь параметры:

   ```mdl
   type List<T> = Empty(unit) | Cons(T, List<T>)
   func len<T>(l: List<T>) -> int:
       ...
   ```

2. В solver-е параметрические функции инстанцируются по фактическим типам
   аргументов и ожидаемому типу результата.
3. Неизвестные параметры представлены opaque-типами до момента уточнения.
4. Полного Hindley-Milner вывода типов нет. Аннотации параметров функций и
   сущностей остаются обязательными.

### 5.5. Коллекции как stdlib ADT

Решения:

1. `List`, `Set`, `Map`, `Option` определены в stdlib:
   - `std/collections/list.mdl`: `List<T> = Empty(unit) | Cons(T, List<T>)`;
   - `std/collections/set.mdl`: `Set<T> = Empty(unit) | Insert(T, Set<T>)`;
   - `std/collections/map.mdl`: `Map<K, V> = Empty(unit) | Put(K, V, Map<K, V>)`;
   - `std/collections/option.mdl`: `Option<T> = None(unit) | Some(T)`.
2. Эти типы должны импортироваться явно.
3. Конструкторы коллекций являются обычными ADT-конструкторами.
4. Такой подход сохраняет единообразную семантику: коллекции не являются
   отдельным встроенным языковым механизмом.

## 6. Термы, функции и чистые вычисления

### 6.1. Top-level values

Поддержаны `val` и `let` как верхнеуровневые декларации значения:

```mdl
val inspection_tags = List.Cons(1, List.Cons(2, List.Cons(3, List.Empty(()))))
```

Решения:

1. `val` и top-level `let` парсятся в один AST-узел `ValueDecl`.
2. Значение может иметь явную type annotation.
3. В runtime top-level values вычисляются до применения фактов.
4. В solver top-level values компилируются как значения на времени `0`, если
   они не зависят от временных сущностей.

### 6.2. Functions

Функция имеет явные параметры, тип результата и чистое тело:

```mdl
func pipe_ok(pipe: Pipe) -> bool:
    let geometry_ok = pipe.length > 0 and pipe.radius > 0
    let pressure_ok = if pipe.pressure > 0 then true else false
    geometry_ok and pressure_ok
```

Решения:

1. Функции считаются чистыми, тотальными и детерминированными.
2. Функции не должны содержать темпоральные операторы. Linter выдает ошибку
   `temporal-in-function`.
3. Рекурсия разрешена для самой функции. Forward reference к более поздней
   функции не разрешен order-aware checker-ом.
4. Параметры функций являются pattern-параметрами, но solver может определить
   рекурсивную Z3-функцию только когда все параметры являются простыми
   переменными.
5. Runtime исполняет пользовательские функции рекурсивно и детерминированно.
6. Solver использует `RecFunction` для определимых рекурсивных функций и
   uninterpreted function для функций, которые не может определить напрямую.

### 6.3. Expressions

Поддержанные выражения:

1. literals: string, int, decimal, rational, bool;
2. names и qualified names;
3. calls;
4. field access;
5. index access;
6. unary `not`, unary `-`;
7. arithmetic `+`, `-`, `*`, `/`, `%`;
8. comparisons `=`, `==`, `!=`, `<`, `<=`, `>`, `>=`;
9. boolean `and`, `or`, `implies`, `->`, `iff`, `<->`;
10. `if ... then ... else ...`;
11. expression-level `let ... in ...`;
12. `case` / `switch`;
13. named record construction;
14. tuple construction;
15. bounded quantifiers `forall` и `exists`;
16. temporal expressions.

### 6.4. Operator precedence

Принята следующая шкала приоритетов, от низкого к высокому:

1. `implies`, `->` - right associative;
2. `iff`, `<->`;
3. `or`;
4. `and`;
5. `until`, `release`, `weak_until`;
6. equality и comparisons;
7. `+`, `-`;
8. `*`, `/`, `%`;
9. prefix unary и temporal prefix;
10. postfix call, field, index.

### 6.5. Pattern matching

Примеры:

```mdl
case tags:
| List.Cons(tag, rest):
    if tag > 0 then positive_tags(rest) else false
| List.Empty(()):
    true
```

Решения:

1. Поддержаны wildcard `_`, variable pattern, literal pattern,
   constructor pattern, record pattern и tuple pattern.
2. Guards задаются через `when`.
3. `case` должен содержать хотя бы один arm.
4. Runtime сообщает ошибку для non-exhaustive match.
5. Solver компилирует pattern matching в условные выражения Z3, где это
   поддержано.
6. Списки сопоставляются через ADT-конструкторы `List.Cons` и `List.Empty(())`, а
   не через синтаксис `[]`.

## 7. Сущности, события, факты и утверждения

### 7.1. Entity

```mdl
entity pipe: Pipe
entity email: string
```

Решения:

1. `entity` представляет состояние моделируемого мира.
2. В solver-е entity получает значение для каждого момента конечной трассы.
3. Поля entity доступны через обычный field access.
4. Entity может иметь clauses `key(...)` и `where ...`.
5. `where` кодируется solver-ом как инвариант для всех моментов горизонта.
6. `key` сейчас является декларативной информацией и проверяется синтаксически,
   но не задает отдельного ограничения в solver-е.

### 7.2. Event

```mdl
event email_received(email: string)
event pressure_changed(pressure: int)
```

Решения:

1. Event задает временное событие.
2. Событие без полей кодируется как boolean symbol на каждом time step.
3. Событие с полями кодируется как Z3 predicate/function:
   `fields -> bool`.
4. События не имеют persistence-семантики по умолчанию.
5. Событие может использоваться в темпоральных формулах.

### 7.3. Facts

```mdl
fact pipe = Pipe { length = 10, radius = 2 }
fact fibonacci_number > 200
```

Решения:

1. Targeted fact `fact x = value` присваивает или ограничивает entity/value.
2. Для entity targeted fact применяется ко всем моментам горизонта.
3. Для top-level value targeted fact применяется к значению.
4. Bare fact без target трактуется как формула.
5. Runtime применяет факты после вычисления top-level values.
6. Solver отслеживает facts в unsat-core.

### 7.4. Assert

```mdl
assert email_is_correct(email) eventually
```

Решения:

1. `assert` входит в AST, linter и core JSON.
2. Bounded solver в текущей реализации игнорирует `assert`.
3. Это разделяет проверяемые нормативные ограничения (`rule`, `fact`) и
   декларативные/исследовательские утверждения, которые могут использоваться
   будущими backend-ами.

## 8. Темпоральный уровень LTLf

### 8.1. Операторы

Поддержаны операторы над конечными трассами:

```text
always, eventually, next, weak_next, never,
until, release, weak_until
```

Решения:

1. Unary temporal operators поддержаны в prefix и postfix форме:

   ```mdl
   always x
   x always
   ```

2. `never p` транслируется как `not eventually p`.
3. `last` является builtin-термом, истинным на последнем шаге горизонта.
4. `next p` на последнем шаге ложно.
5. `weak_next p` на последнем шаге истинно.
6. `eventually p` - дизъюнкция по суффиксу конечной трассы.
7. `always p` - конъюнкция по суффиксу конечной трассы.
8. `until`, `weak_until`, `release` кодируются через стандартные конечные
   развертки.

### 8.2. Отделение темпорального и термового уровней

Решения:

1. Чистые функции не содержат темпоральных операторов.
2. Runtime является point-wise: он может вычислять чистые выражения и факты, но
   не является LTLf-интерпретатором.
3. Runtime для temporal unary возвращает значение операнда point-wise, а
   temporal binary считает неподдерживаемым.
4. Core translator и solver являются местом темпоральной семантики.
5. Boolean term expression в temporal context поднимается в атом.

### 8.3. Quantifiers

```mdl
forall pipe in pipes: pipe.length > 0 always
```

Решения:

1. Quantifiers являются bounded quantifiers.
2. Domain должен быть конечной коллекцией.
3. Solver требует concrete list для домена.
4. `forall` по пустой коллекции истинно.
5. `exists` по пустой коллекции ложно.

## 9. Деонтический уровень

### 9.1. Rule syntax

Основная форма:

```mdl
rule O name: body
rule F name: body
rule P name: body
rule O name when antecedent: body otherwise fallback
strict rule O name: body
defeater rule F name: body
```

Решения:

1. `O` - obligation.
2. `P` - permission.
3. `F` - forbidden.
4. Модальность является частью правила, а не частью выражения.
5. Имя правила нужно для трассируемости, приоритетов и unsat-core.
6. Anonymous rules допускаются, но linter предупреждает, что именованные
   правила предпочтительнее.
7. Если правило не имеет модальности, linter выдает warning `missing-modality`.
8. Если тело правила не содержит явного temporal operator, linter выдает
   warning `rule-without-temporal`.

### 9.2. Strength

Поддержаны три силы правила:

```text
strict, defeasible, defeater
```

Решения:

1. Default strength - `defeasible`.
2. `strict` правило не может быть побеждено priority/override-цепью.
3. `defeasible` правило задает требование, но может быть побеждено более
   приоритетным применимым правилом.
4. `defeater` не накладывает собственного требования, но может блокировать
   нижестоящее правило.

### 9.3. Priority и override

```mdl
override maintenance_exception > perform_work
```

Решения:

1. `priority` и `override` парсятся как одна декларация `PriorityDecl`.
2. Formatter печатает каноническую форму `override`.
3. Цепочка `a > b > c` создает последовательные отношения превосходства.
4. Priority применяется к правилам, которые удалось разрешить по имени.
5. Ссылка на неизвестное правило дает warning в linter и ошибку в solver-е.
6. Более высокое правило побеждает нижнее только если оно применимо.

### 9.4. Applicability и otherwise

Решения:

1. `when` задает условие применимости правила.
2. При отсутствии `when` правило применимо всегда.
3. Solver выбирает winning rules через labels: применимое правило выигрывает,
   если нет применимого более высокого правила.
4. `otherwise` задает альтернативное требование, если antecedent правила
   ложно.
5. `otherwise` интерпретируется с той же модальностью, что и основное тело
   правила.

### 9.5. Семантика модальностей в solver-е

Решения текущего bounded solver-а:

1. `O body` требует `body`.
2. `F body` требует `not body`.
3. `P body` по умолчанию в режиме `--permission strong` требует `body`.
4. `P body` в режиме `--permission ignore` не накладывает требования.
5. `defeater` не накладывает requirement независимо от модальности.

Эту часть следует явно описывать в статье как backend-specific policy, а не как
единственно возможную семантику разрешения.

## 10. Alignments

### 10.1. Declarative align

```mdl
align email to <urn:ietf:rfc:2822:addr-spec> equivalent
align pipe to tube related
```

Решения:

1. `align` связывает subject текущей модели с target.
2. Target может быть qualified name, string или IRI literal `<...>`.
3. Виды связи: `equivalent`, `broader`, `narrower`, `related`.
4. Если вид не указан, используется `equivalent`.
5. Declarative align попадает в AST и core JSON.
6. Solver игнорирует declarative align. Ограничения между моделями задаются
   отдельными alignment-модулями с обычными правилами.

### 10.2. Heuristic aligner

Решения:

1. Aligner проектирует модуль в набор alignment elements: entity и record
   fields, включая вложенные поля.
2. Для entity учитываются имя, поля, тип и path.
3. Для field учитываются имя, тип, parent и path.
4. Встроенный matcher использует token similarity, Jaccard/SequenceMatcher и
   нормализацию типов.
5. Optional external matchers поддержаны через COMA-style backend-и
   `valentine:coma_py` и `bdikit:coma`.
6. При `matcher=auto` внешние matcher-ы пробуются первыми, затем используется
   builtin fallback.
7. Accepted alignments выбираются one-to-one по порогу.
8. Aligner возвращает JSON report и может сгенерировать MDL alignment module.

### 10.3. Alignment module as normative bridge

Сгенерированный alignment-модуль выглядит так:

```mdl
module alignment_pipe_spec_tube

import "./pipe.mdl" as m1
import "./tube.mdl" as m2

# align kind=field score=0.800 matcher=builtin
rule O alignment_001: m1.pipe.length = m2.tube.length always
```

Решения:

1. Alignment является обычным MDL-модулем.
2. Каждое принятое соответствие превращается в obligation rule.
3. Тело правила - temporal equality под `always`.
4. Это позволяет solver-у проверять межмодельную согласованность тем же
   механизмом, что и обычные нормативные правила.
5. Provenance matcher-а сохраняется как комментарий.

## 11. Core translation

Core translator строит JSON-представление:

```json
{
  "language": "MDL-DDL-LTLf-Core",
  "version": "0.1",
  "module": "...",
  "rules": [],
  "atoms": {}
}
```

Решения:

1. Core JSON backend-agnostic.
2. Все импорты, типы, values, entities, events, rules, priorities, facts,
   asserts и alignments сохраняются как отдельные списки.
3. Функциональные тела не компилируются в core. Функции описываются
   декларативно, а вызовы boolean-функций могут подниматься в atoms.
4. Temporal operators отображаются:
   - `always` -> `G`;
   - `eventually` -> `F`;
   - `next` -> `X`;
   - `weak_next` -> `WX`;
   - `until` -> `U`;
   - `release` -> `R`;
   - `weak_until` -> `W`;
   - `never p` -> `not F p`.
5. Boolean connectives отображаются в `and`, `or`, `implies`, `iff`, `not`.
6. Non-temporal boolean term становится atom-ом с символом и исходным текстом.
7. Для rules сохраняются `strength`, `modality`, `antecedent`, `otherwise`,
   `anonymous`, annotations и source span.

## 12. Runtime

Runtime предназначен для point-wise вычислений:

```bash
mdl run examples/email.mdl --expr 'email_is_correct(email)'
```

Решения:

1. Runtime исполняет top-level values, facts, чистые функции, records, tuples,
   ADT constructors, pattern matching и простые stdlib-конструкторы.
2. Runtime не моделирует полную темпоральную семантику.
3. Runtime хранит records как Python dict.
4. ADT constructors в runtime представлены стабильными symbolic values:
   tuple-структурами для конструкторов с аргументами.
5. Stdlib collections имеют Python-представления:
   - `List.Empty(())` -> `[]`;
   - `List.Cons(x, xs)` -> list;
   - `Set.Empty(())` -> `set()`;
   - `Set.Insert(x, xs)` -> set;
   - `Map.Empty(())` -> `{}`;
   - `Map.Put(k, v, m)` -> dict;
   - `Option.None(())` -> `"None"`;
   - `Option.Some(x)` -> `("Some", (x,))`.
6. Runtime используется solver-ом для частичного вычисления concrete
   выражений, когда это безопасно.

## 13. Bounded Z3 solver

### 13.1. Общая модель

Solver проверяет модель на конечной трассе:

```bash
mdl solve examples/pipe.mdl --horizon 1
mdl solve examples/pipe.mdl examples/tube.mdl examples/alignment.mdl --horizon 1
```

Решения:

1. Проверяется один заданный horizon через `--horizon K` или последовательность
   `1..N` через `--max-horizon N`.
2. Минимальный horizon - 1.
3. Solver принимает несколько модулей.
4. Импорты собираются из входных модулей и stdlib.
5. Alignment-модули не имеют специального статуса: они являются обычными
   модулями с правилами.
6. `assert` и declarative `align` в solver-е игнорируются.
7. Выход solver-а всегда JSON с `status`, `horizon`, `checked_horizons`,
   `conflicts`, `model`, `diagnostics`.

### 13.2. Кодирование типов

Решения:

1. `bool`, `int`, `rat/decimal`, `string` кодируются в соответствующие Z3
   sorts.
2. Records и tuples кодируются как product datatypes.
3. Sum types кодируются как Z3 datatypes.
4. Рекурсивные datatypes поддержаны через `CreateDatatypes`.
5. Неизвестные или внешние типы кодируются как opaque sorts.
6. Generic ADT инстанцируются по типовым аргументам.

### 13.3. Кодирование значений и функций

Решения:

1. Entity получает fresh value на каждый момент времени.
2. Top-level value компилируется один раз.
3. Events получают fresh boolean predicate на каждый момент времени.
4. User functions с var-параметрами могут быть добавлены как recursive Z3
   definitions.
5. Функции с более сложными pattern-параметрами могут остаться
   uninterpreted.
6. Concrete runtime values конвертируются обратно в Z3 values.
7. Symbolic index access в общем случае не поддержан; литеральный индекс в
   конкретном list literal поддержан только внутренне.
8. List literals и set literals не являются solver primitives. Для коллекций
   используются stdlib ADT-конструкторы.

### 13.4. Кодирование правил

Решения:

1. Для каждого правила создается label.
2. Label implies applicability.
3. Applicability and absence of higher applicable defeaters implies label.
4. Label implies rule requirement.
5. Higher applicable rules блокируют lower rules согласно priority chain.
6. Strict lower rule не добавляется в defeat pair.
7. Defeater label может побеждать нижние правила, но requirement у defeater
   равен `true`.
8. `otherwise` кодируется как implication от `not applicability` к
   альтернативному requirement.

### 13.5. Conflict reporting

Решения:

1. Все основные ограничения добавляются через `assert_and_track`.
2. Unsat-core возвращает tracked constraints.
3. Conflict payload содержит:
   - id core-а;
   - horizon;
   - rules;
   - facts;
   - constraints с kind, module, name, line, column;
   - raw `z3_core`.
4. Это делает solver пригодным для объяснимого анализа противоречий.

### 13.6. Model reporting

Для `sat` solver возвращает:

1. trace по time steps;
2. значения entities;
3. значения events без полей;
4. `winning_rules`;
5. `defeated_rules`.

Для record, tuple и sum values модель разворачивается обратно в JSON-подобные
структуры.

## 14. Linter и статическая проверка

Linter выполняет легкую статическую проверку, не претендуя на полный типовой
вывод.

Решения:

1. Parse error превращается в diagnostic `parse-error`.
2. Дубликаты деклараций выявляются по виду декларации и имени.
3. Имена проверяются в порядке объявления.
4. Типы тоже должны быть объявлены до использования.
5. Саморекурсия функции разрешена, forward call к более поздней функции
   запрещен.
6. Импорты разрешаются через `ImportResolver`.
7. Поля records и tuples проверяются, если тип известен.
8. Record constructor проверяется на missing, unknown и duplicate fields.
9. Конструкторы ADT регистрируются из sum types.
10. Rules проверяются на:
    - duplicate rule name;
    - missing modality;
    - anonymous rule;
    - absence of explicit temporal operator.
11. Priority проверяется на unknown rule.
12. Declarative align проверяется на subject, который должен разрешаться в
    текущем модуле или через имя модуля.
13. Functions проверяются на наличие финального выражения и запрет temporal
    operators.

Главная методологическая идея: linter должен ловить типовые ошибки
моделирования до запуска solver-а, но не должен требовать полного доказательства
типовой корректности.

## 15. LSP и редакторская поддержка

Решения:

1. LSP-сервер минимален и не имеет внешних зависимостей.
2. Он поддерживает `initialize`, `shutdown`, `didOpen`, `didChange`,
   `completion`.
3. Diagnostics публикуются через `lint_source`.
4. Completion предлагает:
   - ключевые слова;
   - видимые term names;
   - type names в type contexts;
   - record fields после `expr.`;
   - record constructor fields внутри `TypeName { ... }`.
5. LSP может использовать открытые документы для разрешения импортов, даже если
   импортируемый файл еще не сохранен на диске.
6. VS Code TextMate grammar и Tree-sitter grammar существуют для редакторской
   интеграции, но не заменяют авторитетный Python parser.

## 16. Python-first construction

`ModelBuilder` поддерживает построение модели напрямую из Python:

```python
from mdl.builder import ModelBuilder, ref, call, always

m = ModelBuilder("email")
m.entity("email", "string")
m.rule(
    name="email_addr_spec_correct",
    modality="O",
    body=always(call("email_is_correct", ref("email"))),
)

print(m.to_source())
```

Решения:

1. Внешний inference-пайплайн может строить `mdl.ast.Module` без генерации
   текста вручную.
2. Builder использует тот же AST, что и parser.
3. `to_source()` печатает канонический MDL.
4. `from_python()` принимает словари с ключами `module`, `imports`, `types`,
   `entities`, `events`, `rules`, `facts`.
5. Это решение важно для LLM-assisted pipelines: модель можно сначала извлечь в
   Python-структуру, затем стабилизировать как MDL-текст.

## 17. Pretty-printer и каноническая форма

Решения:

1. Formatter печатает один стабильный стиль.
2. `public` не печатается явно, `private` печатается.
3. `priority` и `override` канонически печатаются как `override`.
4. Default `defeasible` strength не печатается.
5. Rule syntax всегда печатается через `:`.
6. Record constructors печатаются как `Type { field = value }`.
7. Temporal postfix сохраняется, если AST node имеет `position="postfix"`.
8. Форматтер является частью дизайна языка, потому что задает canonical source
   для моделей, построенных программно.

## 18. Record equality and schema alignment

Отдельное решение касается равенства record-like values в solver-е.

Решения:

1. Для records одного структурного типа равенство сводится к равенству полей.
2. Для records разных форм solver сравнивает общие поля.
3. Если общих полей нет, record equality не добавляет содержательного
   ограничения.
4. Это сделано для межмодельного выравнивания, где разные домены могут иметь
   несовпадающие record-типы.
5. Field-level alignment rules должны использоваться для точного связывания
   несовпадающих схем, например `pipe.radius = tube.r always`.

Это решение нужно явно раскрывать в статье, поскольку оно отличается от
строгого номинального равенства записей.

## 19. Ограничения текущей версии

Ограничения являются частью дизайна v3, а не случайными недоделками:

1. Нет полного type inference.
2. Нет effect system, mutation, I/O и пользовательских side effects.
3. Нет полноценной runtime-семантики LTLf.
4. Solver bounded, а не unbounded model checker.
5. `assert` пока не участвует в solver-е.
6. Declarative `align` не является solver constraint.
7. Collection literals не являются каноническим текстовым синтаксисом.
8. Stdlib должен импортироваться явно.
9. Symbolic quantifiers ограничены конечными concrete domains.
10. Symbolic index access в общем случае не поддержан solver-ом.
11. Публичная/приватная видимость задает API-поверхность и tooling-поведение,
    но не является полноценной security boundary.
12. Tree-sitter grammar - scaffold для редакторов, а не источник истины.

## 20. Краткая матрица проектных решений

| Область | Решение | Обоснование |
| --- | --- | --- |
| Языковой класс | DSL для нормативного моделирования | Уменьшает семантическую неопределенность |
| Вычисления | Чистые deterministic functions | Позволяет runtime и solver переиспользовать термы |
| Темпоральность | LTLf на конечных трассах | Совместимо с bounded проверкой |
| Деонтика | Rules с O/P/F, strength и priority | Моделирует нормы, запреты и исключения |
| Синтаксис rules | `rule O name: body` | Отделяет декларацию от равенства термов |
| Records | Номинальный constructor, структурные поля | Улучшает читаемость и проверку полей |
| Collections | Stdlib ADT, явный import | Убирает неявные встроенные коллекции |
| Imports | Alias + exposing | Поддерживает межмодельные alignment-модули |
| AST | Публичные dataclasses | Поддерживает Python-first extraction |
| Formatter | Канонический source | Стабилизирует модели и тесты |
| Linter | Order-aware lightweight checks | Ловит ошибки до solver-а |
| Runtime | Point-wise interpreter | Проверяет чистые термы без LTLf |
| Core | Backend-agnostic JSON | Не замыкает язык на Z3 |
| Solver | Bounded Z3 encoding | Дает witness trace и unsat-core |
| Alignment | Heuristic report + rule module | Делает schema matching проверяемым |
| LSP | Minimal stdio server | Дает практическую редакторскую поддержку |

## 21. Позиционирование для научной статьи

Главные тезисы, которые следует вынести в статью:

1. `mdl` соединяет функциональное моделирование домена, LTLf и defeasible
   deontic rules в одном компактном DSL.
2. Язык сознательно разделяет pure computation, temporal formulas и normative
   rule semantics.
3. Публичный AST и builder делают язык пригодным для automatic/LLM-assisted
   extraction pipelines.
4. Alignment-модули превращают heuristic schema matching в проверяемые
   нормативные ограничения.
5. Bounded solver возвращает не только `sat/unsat`, но и witness trace,
   winning/defeated rules и unsat-core с source spans.
6. Ограничения языка являются методологическими: они уменьшают пространство
   неоднозначностей и делают модели пригодными для объяснимой проверки.

## 22. Опорные артефакты реализации

Для проверки решений в коде:

1. `src/mdl/lexer.py` - токенизация, ключевые слова, отступы, комментарии.
2. `src/mdl/parser.py` - авторитетный parser и синтаксические запреты.
3. `src/mdl/ast.py` - публичный AST.
4. `src/mdl/printer.py` - canonical formatting.
5. `src/mdl/linter.py` - order-aware checker и diagnostics.
6. `src/mdl/runtime.py` - point-wise runtime.
7. `src/mdl/core.py` - backend-agnostic DDL-LTLf core translation.
8. `src/mdl/solver.py` - bounded Z3 encoding.
9. `src/mdl/aligner.py` - semantic alignment and alignment module rendering.
10. `src/mdl/builder.py` - Python-first model construction.
11. `src/mdl/lsp.py` - stdio LSP.
12. `src/mdl/stdlib/std/` - стандартные ADT-модули.
13. `examples/` и `tests/fixtures/solver_suite/` - минимальные и расширенные
    сценарии использования.
