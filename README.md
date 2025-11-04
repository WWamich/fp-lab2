
<div align="center">

# Отчёт по лабораторной работе №2
## по курсу "Функциональное программирование"

</div>

---

### **Титульный лист**

|                          |                                                                |
| ------------------------ | -------------------------------------------------------------- |
| **Университет**          | Университет ИТМО                                               |
| **Факультет**            | Факультет программной инженерии и компьютерной техники (ПИиКТ) |
| **Направление**          | Программная инженерия                                          |
| **Дисциплина**           | Функциональное программирование                                |
| **Студент**              | Дмитриев Денис Сергеевич                                            |
| **Группа**               | P3312                             |
| **Преподаватель**        | Сиразетдинов Азат Ниязович                     |
| **Дата сдачи**           | 04.11.2025                            |

<br/>

---

### **1. Требования к разработанному ПО**
Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing), а также разделением интерфейса и особенностей реализации.
В рамках лабораторной работы вам предлагается реализовать одну из предложенных классических структур данных (список, дерево, бинарное дерево, hashmap, граф...).
Требования:

Функции:

добавление и удаление элементов;
фильтрация;
отображение (map);
свертки (левая и правая);
структура должна быть моноидом.


Структуры данных должны быть неизменяемыми.
Библиотека должна быть протестирована в рамках unit testing.
Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
Структура должна быть полиморфной.
Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
Обратите внимание:

API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь нужно протестировать именно API (dict, set, bag).
Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим сравнением), реализованная на уровне API, а не внутреннего представления.
Вариант: oa-dict

### **2 . Ключевые элементы реализации с минимальными комментариями**



#### **Определение типов:**
``` haskell
data Slot k v
  = Empty
  | Deleted
  | Occupied k v
  deriving (Show, Eq)

data OADict k v = OADict
  { dictSize :: Int,
    dictSlots :: V.Vector (Slot k v)
  }
  deriving (Show)
``` 
Cтруктруа OADict хранит не только массив Vector, но и текущий размер(тогда сможем получить за O(1)). Три состояния стандартные для OpenAddress словаря - Deleted это надгробие для сохранения целостности цепочки проб при удалении. 
#### **Вставка элемента и рехэширование:**
``` haskell
insert :: (Eq k, Hashable k) => k -> v -> OADict k v -> OADict k v
insert key value dict
  | needsResize dict = insert key value (resize dict)
  | otherwise =
      let slots = dictSlots dict
          capacity = V.length slots
          startIndex = hash key `mod` capacity
          go i = case slots V.! i of
            Empty ->
              let newSlots = slots V.// [(i, Occupied key value)]
               in OADict (dictSize dict + 1) newSlots
            Deleted ->
              let newSlots = slots V.// [(i, Occupied key value)]
               in OADict (dictSize dict + 1) newSlots
            Occupied k _
              | k == key ->
                  let newSlots = slots V.// [(i, Occupied key value)]
                   in OADict (dictSize dict) newSlots
              | otherwise -> go ((i + 1) `mod` capacity)
       in go startIndex

needsResize :: OADict k v -> Bool
needsResize dict =
  let capacity = fromIntegral $ V.length (dictSlots dict)
      size = fromIntegral $ dictSize dict
   in size * 4 >= capacity * 3

resize :: (Eq k, Hashable k) => OADict k v -> OADict k v
resize dict =
  let oldPairs = toList dict
      newCapacity = V.length (dictSlots dict) * 2
      newEmptyDict = OADict 0 (V.replicate newCapacity Empty)
   in foldr (\(k, v) accDict -> insert k v accDict) newEmptyDict oldPairs
```
insert - классическая иммутабельности хаскьюэля. Перед вставкой происходит проверка на необходимость рехеширования для поддержания амортизированной сложности O(1). resize являяется гарантией того, что структура останется эффективной даже при большом количестве элементов.
#### **Абстракция API: публичный и внутренний модули**
``` haskell
module Dict
  ( OADict,
    empty,
    lookup,
    toList,
    insert,
    delete,
    fromList,
    mapDict,
    filterDict,
    foldrDict,
    foldlDict,
  )
where

import Data.Dict.Internal
 

 module Data.Dict.Internal
  ( OADict (..), 
    Slot (..),
  )
where
```
Чтобы было непротекающее API, я использую два модуля. Data.Dict.Internal содержит в себе 
всю реализацию. В свою очередь Data.Dict импортирует только безопасный API, скрывая внутреннюю структуру от пользователя. 
#### **Эффективное сравнение на равенство**
``` haskell 
instance (Eq k, Hashable k, Eq v) => Eq (OADict k v) where
  d1 == d2
    | dictSize d1 /= dictSize d2 = False
    | otherwise =
        all (\(k, v) -> lookup k d2 == Just v) (toList d1)
```
Вместо наивного приведения к спискам и сортировки, сначала происходит быстрая O(1) проверка по размеру. Затем для каждого  элемента первого словаря проверяется его наличие во втором. Это гарантия семантического равенства, не зависящего от порядка элементов или наличия надгробий. 

#### **Реализация Monoid**
``` haskell
instance (Eq k, Hashable k) => Semigroup (OADict k v) where
  (<>) = union

instance (Eq k, Hashable k) => Monoid (OADict k v) where
  mempty = empty
```
Для OADict реализован Monoid, где mempty - пустой словарь, а операция объединения - это право-смещенное объединение двух словарей. 
### **3 . Тесты, отчёт инструмента тестирования, метрики**
Для сборки и тестирования использовалась система stack. Тестирования проводилось при помощи tasty, который объединяет возможности юнит-тестирования и property-based тестирования.
#### **Примеры Unit-тестов:**
``` haskell
  testCase "delete correctly handles collisions" $
        let dict :: OADict Int Int
            dict = insert 17 170 (insert 1 10 empty)
            dict' = delete 1 dict
         in do
              lookup 1 dict' @?= Nothing
              lookup 17 dict' @?= Just 170
              dictSize dict' @?= 1,
   testCase "Eq instance works for maps with different insertion order" $
        let dict1 = fromList [("a", 1), ("b", 2)]
            dict2 = fromList [("b", 2), ("a", 1)]
         in dict1 @?= dict2,
```
Как видно по этим тестам, я использовал юнит-тесты для покрытия корнер-кейсов, которые трудно поймать случайной генерацией, по типу удаления элемента, который стоит первым в цепочке коллизий.  
#### **Примеры Property-based тестов:**
``` haskell
-- 1. Свойство вставки: после вставки (k, v) поиск по k всегда находит v.
testProperty "lookup k (insert k v dict) == Just v" $
  \(key :: String) (value :: Int) (dict :: OADict String Int) ->
    lookup key (insert key value dict) == Just value

-- 2. Свойство удаления: после удаления k поиск по k всегда возвращает Nothing.
testProperty "lookup k (delete k dict) == Nothing" $
  \(key :: String) (dict :: OADict String Int) ->
    lookup key (delete key dict) == Nothing

-- 3. Свойство моноида (ассоциативность): порядок объединения не влияет на результат.
testProperty "mappend is associative: (a <> b) <> c == a <> (b <> c)" $
  \(a :: OADict String Int) b c ->
    (a <> b) <> c == a <> (b <> c)
```
Я реализовал три теста, объяснил  про что они уже в коде. Они дают высокую уверенность в корректной реализации, автоматически проверяя сотни комбинаций случайных данных на соответтсвие фундаментальным законам(по типу моноида).
#### **Отчёт по итогу**
``` 
Data.Dict Tests
  Unit tests
    lookup on an empty dictionary returns Nothing:                            OK
    lookup after insert finds the element:                                    OK
    inserting an existing key updates the value:                              OK
    size is updated correctly after inserts:                                  OK
    size doesn't change when updating a key:                                  OK
    collision is handled correctly:                                           OK
    delete removes an existing key:                                           OK
    delete does nothing for a non-existent key:                               OK
    delete correctly handles collisions:                                      OK
    mapDict applies a function to all values:                                 OK
    filterDict keeps elements that satisfy the predicate:                     OK
    mappend (<>) combines two dictionaries:                                   OK
    Eq instance works for maps with different insertion order:                OK
    Eq instance works for maps with different internal state (Deleted slots): OK
    Eq instance returns False for different maps:                             OK
  Property tests
    lookup k (insert k v dict) == Just v:                                     OK (0.17s)
      +++ OK, passed 100 tests.
    lookup k (delete k dict) == Nothing:                                      OK (0.15s)
      +++ OK, passed 100 tests.
    mappend is associative: (a <> b) <> c == a <> (b <> c):                   OK (0.37s)
      +++ OK, passed 100 tests.

All 18 tests passed (0.38s)
```

### **4. Выводы**

В ходе выполнения лабораторной работы были освоены и применены на практике ключевые концепции функционального программирования:

*   **Сильная статистическая типизация** это кайф. Оно постоянно указывало на ошибки в моём коде, по типу отсутствия Hashable в Eq из-за забывчивости. Это предотвратило много ошибок.
*   **Неизменяемость тоже кайф** сначало непривычно, затем вошёл во вкус. Оно прям очень простым выглядит и занимает немного кода из-за этого. 
*   **Property-based тестирование.** — юнит-тесты с проверкой корнер-кейсов это хорошо, но зачастую проперти-бейзд тестирования покрывают большое количество кода, тем самым растёт и само покрытия кода тестами. Вместо придумывания примеров говоришь какими свойствами будут обладать тесты. 
*   **Ну и абстракции haskell** тоже кайф что теоретические идеи показали всю свою мощь на практике. Очень легко было реализовывать с полиморфизмом, моноидом и тайпклассами 
