package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.DataCreator;
import ru.itmo.wp.model.domain.DomainEntity;
import ru.itmo.wp.model.exception.RepositoryException;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class BasicRepositoryImpl<TData extends DomainEntity> {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();

    protected void save(TData obj, PreparedStatement statement,
                        String tableName, DataCreator<TData> creator) throws SQLException {
        if (statement.executeUpdate() != 1) {
            throw new RepositoryException("Can't save " + tableName);
        } else {
            ResultSet generatedKeys = statement.getGeneratedKeys();
            if (generatedKeys.next()) {
                obj.setId(generatedKeys.getLong(1));
                obj.setCreationTime(find(obj.getId(), tableName, creator).getCreationTime());
            } else {
                throw new RepositoryException("Can't save" + tableName + " [no autogenerated fields].");
            }
        }
    }

    TData find(long id, String tableName, DataCreator<TData> creator) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM " + tableName + " WHERE id=?")) {
                statement.setLong(1, id);
                try (ResultSet resultSet = statement.executeQuery()) {
                    return creator.create(statement.getMetaData(), resultSet);
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find " + tableName, e);
        }
    }

    TData findBy(String param, String paramName, String tableName, DataCreator<TData> creator) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM " + tableName + " WHERE " + paramName + "=?")) {
                statement.setString(1, param);
                try (ResultSet resultSet = statement.executeQuery()) {
                    return creator.create(statement.getMetaData(), resultSet);
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find " + tableName, e);
        }
    }

    List<TData> findBy(long param, String paramName, String tableName, DataCreator<TData> creator) {
        List<TData> res = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM " + tableName + " WHERE " + paramName + "=? ORDER BY creationTime DESC")) {
                statement.setLong(1, param);
                try (ResultSet resultSet = statement.executeQuery()) {
                    TData item;
                    while ((item = creator.create(statement.getMetaData(), resultSet)) != null) {
                        res.add(item);
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find " + tableName, e);
        }
        return res;
    }

    List<TData> findAll(String tableName, DataCreator<TData> creator) {
        List<TData> res = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM " + tableName + " ORDER BY id DESC")) {
                try (ResultSet resultSet = statement.executeQuery()) {
                    TData item;
                    while ((item = creator.create(statement.getMetaData(), resultSet)) != null) {
                        res.add(item);
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find " + tableName, e);
        }
        return res;
    }
}