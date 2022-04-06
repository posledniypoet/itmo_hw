package info.kgeorgiy.ja.kalinichev.student;

import info.kgeorgiy.java.advanced.student.Group;
import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.GroupQuery;
import info.kgeorgiy.java.advanced.student.Student;

import java.util.*;
import java.util.function.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class StudentDB implements GroupQuery {
    private static final Comparator<Group> GROUP_COMPARATOR = Comparator.comparing(Group::getName);
    private final Comparator<Student> NameComparator = Comparator.comparing(Student::getLastName, Comparator.reverseOrder())
            .thenComparing(Student::getFirstName, Comparator.reverseOrder())
            .thenComparing(Student::getId);

    @Override
    public List<Group> getGroupsByName(Collection<Student> students) {
        return getGroupQuery(students, NameComparator);
    }

    @Override
    public List<Group> getGroupsById(Collection<Student> students) {
        return getGroupQuery(students, Student::compareTo);
    }

    @Override
    public GroupName getLargestGroup(Collection<Student> students) {
        return maxGroupNameQuery(students,
                Comparator.comparingInt((Group g) -> g.getStudents().size()).
                        thenComparing(Comparator.comparing(Group::getName)));
    }

    @Override
    public GroupName getLargestGroupFirstName(Collection<Student> students) {
        return maxGroupNameQuery(students,
                Comparator.comparing((Group g) ->
                        (g.getStudents().stream().map(Student::getFirstName)).distinct().count())
                        .thenComparing(Comparator.comparing(Group::getName).reversed()));
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return students.stream().map(student -> student.getFirstName()).collect(Collectors.toList());
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return students.stream().map(student -> student.getLastName()).collect(Collectors.toList());
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return students.stream().map(student -> student.getGroup()).collect(Collectors.toList());
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return students.stream().map(student -> student.getFirstName() + " " + student.getLastName()).collect(Collectors.toList());
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return new TreeSet<>(students.stream().map(Student::getFirstName).collect(Collectors.toList()));
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream().max(Student::compareTo).map(Student::getFirstName).orElse("");
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortBy(students, Comparator.comparing(Student::getId));
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortBy(students, NameComparator);
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return filterBy(student -> student.getFirstName().equals(name), NameComparator, students);
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return filterBy(student -> student.getLastName().equals(name), NameComparator, students);
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return filterBy(student -> student.getGroup().equals(group), NameComparator, students);
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return students.stream().filter(student -> student.getGroup().equals(group)).collect(Collectors.toMap(Student::getLastName, Student::getFirstName, BinaryOperator.minBy(String::compareTo)));
    }
    @SuppressWarnings("unchecked")
    private List<Student> sortBy(Collection<Student> students, Comparator<Student> comparator) {
        return students.stream()
                .sorted(comparator)
                .collect(Collectors.toList());
    }
    @SuppressWarnings("unchecked")
    private List<Student> filterBy(Predicate<Student> predicate, Comparator<Student> comparator, Collection<Student> collection) {
        return collection.stream().filter(predicate).sorted(comparator).collect(Collectors.toList());
    }


    private static Stream<Group> groupStream(Collection<Student> students,
                                             Function<Map.Entry<GroupName, List<Student>>, Group> groupConstructor) {
        return students
                .stream()
                .collect(Collectors.groupingBy(Student::getGroup, Collectors.toList()))
                .entrySet()
                .stream()
                .map(groupConstructor)
                .sorted(GROUP_COMPARATOR);
    }

    private static List<Group> getGroupQuery(Collection<Student> students, Comparator<Student> comparator) {
        return groupStream(students, (Map.Entry<GroupName, List<Student>> e) -> new Group(
                        e.getKey(), e.getValue().stream().sorted(comparator).collect(Collectors.toList())
                )
        ).collect(Collectors.toList());
    }

    private static GroupName maxGroupNameQuery(Collection<Student> students, Comparator<Group> comparator) {
        return groupStream(students, (Map.Entry<GroupName, List<Student>> e) -> new Group(e.getKey(), e.getValue()))
                .max(comparator)
                .map(Group::getName)
                .orElse(null);
    }


}
